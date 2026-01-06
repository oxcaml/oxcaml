(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Jules Jacobs, Jane Street                             *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Axis lattice: efficient bitfield encoding of jkind axes.

   This module packs 13 axes into an OCaml immediate-sized integer, where each
   axis can have 2 or 3 possible values (levels). The axes are indexed 0-12 and
   their values are ordered from most restrictive (0) to least restrictive
   (max).

   Axis layout (index, name, values from level 0 to max):
   0. Areality (Regionality): Global -> Regional -> Local
   1. Uniqueness (monadic): Aliased -> Unique
   2. Linearity: Many -> Once
   3. Contention (monadic): Contended -> Shared -> Uncontended
   4. Portability: Portable -> Shareable -> Nonportable
   5. Forkable: Forkable -> Unforkable
   6. Yielding: Unyielding -> Yielding
   7. Statefulness: Stateless -> Observing -> Stateful
   8. Visibility (monadic): Immutable -> Read -> Read_write
   9. Staticity (monadic): Dynamic -> Static
   10. Externality: External -> External64 -> Internal
   11. Nullability: Non_null -> Maybe_null
   12. Separability: Non_float -> Separable -> Maybe_separable

   Axes 0-9 are modal axes (affect mode-crossing).
   Axes 10-12 are non-modal axes (externality and shallow axes).
   Axes 11-12 are "shallow" axes (nullability and separability) that are
   sometimes excluded from masking operations.

   Each 3-valued axis uses 2 bits, each 2-valued axis uses 1 bit.
   For 3-valued axes, level 2 is encoded as 0b11 (not 0b10).

   Encoding scheme: each axis uses a "prefix ones" encoding within its slot.
   For 2 levels: 0 -> 0b0, 1 -> 0b1.
   For 3 levels: 0 -> 0b00, 1 -> 0b01, 2 -> 0b11.
   If an axis had 5 levels, it would be 0b0000, 0b0001, 0b0011,
   0b0111, 0b1111.

   With this encoding, per-axis order is just bit inclusion, so bitwise
   OR/AND compute join/meet, and disjoint slots make that true for all
   axes at once. *)

let axis_sizes = [| 3; 2; 2; 3; 3; 2; 2; 3; 3; 2; 3; 2; 3 |]

let num_axes = 13

let axis_by_number = Array.of_list Jkind_axis.Axis.all

(* widths[i] = 2 for size-3 axes, 1 for size-2 *)
let widths =
  Array.map
    (function 3 -> 2 | 2 -> 1 | _ -> invalid_arg "bad axis size")
    axis_sizes

(* consecutive packing offsets *)
let offsets =
  let off = Array.make num_axes 0 in
  let a = ref 0 in
  for i = 0 to num_axes - 1 do
    off.(i) <- !a;
    a := !a + widths.(i)
  done;
  off

(* 1 if axis i has a high bit (i.e. width=2), else 0 *)
let has_hi =
  Array.init num_axes (fun i ->
      match axis_sizes.(i) with
      | 3 -> 1
      | 2 -> 0
      | _ -> invalid_arg "bad axis size")

let lo_mask = Array.map (fun off -> 1 lsl off) offsets

let hi_mask =
  Array.mapi
    (fun i off -> if has_hi.(i) = 1 then 1 lsl (off + 1) else 0)
    offsets

let axis_mask = Array.map2 (fun lo hi -> lo lor hi) lo_mask hi_mask

(* OR of all low bits (for size-2 axes that’s their only bit).
   For this layout: 0x6D75D. *)
let lows = Array.fold_left ( lor ) 0 lo_mask

type t = int

let bot : t = 0

(* For this layout top happens to be all 20 bits set: 0xF_FFFF. *)
let top : t = Array.fold_left ( lor ) 0 axis_mask

let join (a : t) (b : t) : t = a lor b

let meet (a : t) (b : t) : t = a land b

let leq (a : t) (b : t) : bool = a land b = a

let equal (a : t) (b : t) : bool = a = b

let hash = Hashtbl.hash

(* Branchless get: for 3-ary axes level = lo + hi (00→0, 01→1, 11→2).
    For 2-ary axes hi=0 (masked by has_hi). *)
let get_axis (v : t) ~axis:i : int =
  let off = offsets.(i) in
  let lo = (v lsr off) land 1 in
  let hi = (v lsr (off + 1)) land has_hi.(i) in
  lo + hi

(* Branchless set:
    low_bit  = (lev | (lev >> 1)) & 1  (0→0, 1→1, 2→1)
    high_bit = (lev >> 1) & has_hi
      (0→0, 1→0, 2→1; zeroed for 1-bit axes)
    No range checks—caller keeps lev in-range. *)
let set_axis (v : t) ~axis:i ~level:lev : t =
  let off = offsets.(i) in
  let cleared = v land lnot axis_mask.(i) in
  let lo = lev lor (lev lsr 1) land 1 in
  let hi = (lev lsr 1) land has_hi.(i) in
  cleared lor (lo lsl off) lor (hi lsl (off + 1))

let encode ~levels : t =
  let _, v =
    Array.fold_left
      (fun (i, acc) lev -> i + 1, set_axis acc ~axis:i ~level:lev)
      (0, 0) levels
  in
  v

let decode (v : t) : int array =
  Array.init num_axes (fun i -> get_axis v ~axis:i)

let non_bot_axes (v : t) : int list =
  let rec loop i acc =
    if i = num_axes
    then List.rev acc
    else
      let acc' = if v land axis_mask.(i) <> 0 then i :: acc else acc in
      loop (i + 1) acc'
  in
  loop 0 []

let of_levels ~(levels : int array) : t = encode ~levels

let to_levels (v : t) : int array = decode v

let pp (v : t) : string =
  let lv = decode v |> Array.to_list |> List.map string_of_int in
  "[" ^ String.concat "," lv ^ "]"

let to_string = pp

(* Axis-wise residual:
    r = a & ~b zeroes axes where b >= a; only invalid per-axis
    pattern is 10 (from 11 - 01).
    (r >> 1) copies surviving high bits down to their own low slots;
    AND with ~ (lows >> 1) kills spillovers from low bits into neighbors;
    OR repairs 10 -> 11. *)

let lnot_lsr_1_lows = lnot (lows lsr 1)

let co_sub (a : t) (b : t) : t =
  let r = a land lnot b in
  r lor ((r lsr 1) land lnot_lsr_1_lows)

(* Build a mask from a set of relevant axes. *)
let of_axis_set (set : Jkind_axis.Axis_set.t) : t =
  let levels = Array.make num_axes 0 in
  let open Jkind_axis in
  (* Iterate in axis_index order. *)
  List.iteri
    (fun i (Axis.Pack ax) ->
      if Axis_set.mem set ax then levels.(i) <- axis_sizes.(i) - 1)
    Axis.all;
  encode ~levels

(* IK-only: compute relevant axes of a constant modality, mirroring
   Jkind.relevant_axes_of_modality. *)
let relevant_axes_of_modality
    ~(relevant_for_shallow : [`Relevant | `Irrelevant])
    (modality : Mode.Modality.Const.t) : Jkind_axis.Axis_set.t =
  Jkind_axis.Axis_set.create ~f:(fun ~axis:(Jkind_axis.Axis.Pack axis) ->
      match axis with
      | Modal axis ->
        let (Mode.Modality.Axis.P axis_for_modality) =
          Mode.Crossing.Axis.(P axis |> to_modality)
        in
        let modality_on_axis =
          Mode.Modality.Const.proj axis_for_modality modality
        in
        not
          (Mode.Modality.Per_axis.is_constant axis_for_modality modality_on_axis)
      | Nonmodal Externality -> true
      | Nonmodal (Separability | Nullability) -> (
        match relevant_for_shallow with
        | `Relevant -> true
        | `Irrelevant -> false))

(* Mask that excludes the shallow axes (nullability and separability). *)
let mask_shallow : t = co_sub top (join axis_mask.(11) axis_mask.(12))

(* Directly produce an axis-lattice mask from a constant modality. *)
let mask_of_modality ~(relevant_for_shallow : [`Relevant | `Irrelevant])
    (modality : Mode.Modality.Const.t) : t =
  relevant_axes_of_modality ~relevant_for_shallow modality |> of_axis_set

(* Helpers to translate between axis enumerations and packed levels. *)
module Levels = struct
  let level_of_areality (a : Mode.Regionality.Const.t) : int =
    match a with
    | Mode.Regionality.Const.Global -> 0
    | Mode.Regionality.Const.Regional -> 1
    | Mode.Regionality.Const.Local -> 2

  let level_of_linearity (x : Mode.Linearity.Const.t) : int =
    match x with
    | Mode.Linearity.Const.Many -> 0
    | Mode.Linearity.Const.Once -> 1

  let level_of_uniqueness_monadic (x : Mode.Uniqueness.Const.t) : int =
    match x with
    | Mode.Uniqueness.Const.Aliased -> 0
    | Mode.Uniqueness.Const.Unique -> 1

  let level_of_portability (x : Mode.Portability.Const.t) : int =
    match x with
    | Mode.Portability.Const.Portable -> 0
    | Mode.Portability.Const.Shareable -> 1
    | Mode.Portability.Const.Nonportable -> 2

  let level_of_contention_monadic (x : Mode.Contention.Const.t) : int =
    match x with
    | Mode.Contention.Const.Contended -> 0
    | Mode.Contention.Const.Shared -> 1
    | Mode.Contention.Const.Uncontended -> 2

  let level_of_forkable (x : Mode.Forkable.Const.t) : int =
    match x with
    | Mode.Forkable.Const.Forkable -> 0
    | Mode.Forkable.Const.Unforkable -> 1

  let level_of_yielding (x : Mode.Yielding.Const.t) : int =
    match x with
    | Mode.Yielding.Const.Unyielding -> 0
    | Mode.Yielding.Const.Yielding -> 1

  let level_of_statefulness (x : Mode.Statefulness.Const.t) : int =
    match x with
    | Mode.Statefulness.Const.Stateless -> 0
    | Mode.Statefulness.Const.Observing -> 1
    | Mode.Statefulness.Const.Stateful -> 2

  let level_of_visibility_monadic (x : Mode.Visibility.Const.t) : int =
    match x with
    | Mode.Visibility.Const.Immutable -> 0
    | Mode.Visibility.Const.Read -> 1
    | Mode.Visibility.Const.Read_write -> 2

  let level_of_staticity_monadic (x : Mode.Staticity.const) : int =
    match x with Mode.Staticity.Dynamic -> 0 | Mode.Staticity.Static -> 1

  let level_of_externality (x : Jkind_axis.Externality.t) : int =
    match x with External -> 0 | External64 -> 1 | Internal -> 2

  let level_of_nullability (x : Jkind_axis.Nullability.t) : int =
    match x with Non_null -> 0 | Maybe_null -> 1

  let level_of_separability (x : Jkind_axis.Separability.t) : int =
    match x with Non_float -> 0 | Separable -> 1 | Maybe_separable -> 2

  let areality_of_level = function
    | 0 -> Mode.Regionality.Const.Global
    | 1 -> Mode.Regionality.Const.Regional
    | 2 -> Mode.Regionality.Const.Local
    | _ -> invalid_arg "Axis_lattice.areality_of_level"

  let linearity_of_level = function
    | 0 -> Mode.Linearity.Const.Many
    | 1 -> Mode.Linearity.Const.Once
    | _ -> invalid_arg "Axis_lattice.linearity_of_level"

  let uniqueness_of_level_monadic = function
    | 0 -> Mode.Uniqueness.Const.Aliased
    | 1 -> Mode.Uniqueness.Const.Unique
    | _ -> invalid_arg "Axis_lattice.uniqueness_of_level_monadic"

  let portability_of_level = function
    | 0 -> Mode.Portability.Const.Portable
    | 1 -> Mode.Portability.Const.Shareable
    | 2 -> Mode.Portability.Const.Nonportable
    | _ -> invalid_arg "Axis_lattice.portability_of_level"

  let contention_of_level_monadic = function
    | 0 -> Mode.Contention.Const.Contended
    | 1 -> Mode.Contention.Const.Shared
    | 2 -> Mode.Contention.Const.Uncontended
    | _ -> invalid_arg "Axis_lattice.contention_of_level_monadic"

  let forkable_of_level = function
    | 0 -> Mode.Forkable.Const.Forkable
    | 1 -> Mode.Forkable.Const.Unforkable
    | _ -> invalid_arg "Axis_lattice.forkable_of_level"

  let yielding_of_level = function
    | 0 -> Mode.Yielding.Const.Unyielding
    | 1 -> Mode.Yielding.Const.Yielding
    | _ -> invalid_arg "Axis_lattice.yielding_of_level"

  let statefulness_of_level = function
    | 0 -> Mode.Statefulness.Const.Stateless
    | 1 -> Mode.Statefulness.Const.Observing
    | 2 -> Mode.Statefulness.Const.Stateful
    | _ -> invalid_arg "Axis_lattice.statefulness_of_level"

  let visibility_of_level_monadic = function
    | 0 -> Mode.Visibility.Const.Immutable
    | 1 -> Mode.Visibility.Const.Read
    | 2 -> Mode.Visibility.Const.Read_write
    | _ -> invalid_arg "Axis_lattice.visibility_of_level_monadic"

  let staticity_of_level_monadic = function
    | 0 -> Mode.Staticity.Dynamic
    | 1 -> Mode.Staticity.Static
    | _ -> invalid_arg "Axis_lattice.staticity_of_level_monadic"

  let externality_of_level = function
    | 0 -> Jkind_axis.Externality.External
    | 1 -> Jkind_axis.Externality.External64
    | 2 -> Jkind_axis.Externality.Internal
    | _ -> invalid_arg "Axis_lattice.externality_of_level"

  let nullability_of_level = function
    | 0 -> Jkind_axis.Nullability.Non_null
    | 1 -> Jkind_axis.Nullability.Maybe_null
    | _ -> invalid_arg "Axis_lattice.nullability_of_level"

  let separability_of_level = function
    | 0 -> Jkind_axis.Separability.Non_float
    | 1 -> Jkind_axis.Separability.Separable
    | 2 -> Jkind_axis.Separability.Maybe_separable
    | _ -> invalid_arg "Axis_lattice.separability_of_level"
end

type boxed = {
  areality : Mode.Regionality.Const.t;
  linearity : Mode.Linearity.Const.t;
  uniqueness : Mode.Uniqueness.Const.t;
  portability : Mode.Portability.Const.t;
  contention : Mode.Contention.Const.t;
  forkable : Mode.Forkable.Const.t;
  yielding : Mode.Yielding.Const.t;
  statefulness : Mode.Statefulness.Const.t;
  visibility : Mode.Visibility.Const.t;
  staticity : Mode.Staticity.const;
  externality : Jkind_axis.Externality.t;
  nullability : Jkind_axis.Nullability.t;
  separability : Jkind_axis.Separability.t;
}

let of_boxed
    ({ areality;
       linearity;
       uniqueness;
       portability;
       contention;
       forkable;
       yielding;
       statefulness;
       visibility;
       staticity;
       externality;
       nullability;
       separability
     } :
      boxed) : t =
  let open Levels in
  let levels =
    [| level_of_areality areality;
       level_of_uniqueness_monadic uniqueness;
       level_of_linearity linearity;
       level_of_contention_monadic contention;
       level_of_portability portability;
       level_of_forkable forkable;
       level_of_yielding yielding;
       level_of_statefulness statefulness;
       level_of_visibility_monadic visibility;
       level_of_staticity_monadic staticity;
       level_of_externality externality;
       level_of_nullability nullability;
       level_of_separability separability
    |]
  in
  of_levels ~levels

let to_boxed (x : t) : boxed =
  let open Levels in
  let lv = to_levels x in
  { areality = areality_of_level lv.(0);
    uniqueness = uniqueness_of_level_monadic lv.(1);
    linearity = linearity_of_level lv.(2);
    contention = contention_of_level_monadic lv.(3);
    portability = portability_of_level lv.(4);
    forkable = forkable_of_level lv.(5);
    yielding = yielding_of_level lv.(6);
    statefulness = statefulness_of_level lv.(7);
    visibility = visibility_of_level_monadic lv.(8);
    staticity = staticity_of_level_monadic lv.(9);
    externality = externality_of_level lv.(10);
    nullability = nullability_of_level lv.(11);
    separability = separability_of_level lv.(12)
  }

let const_of_levels ~areality ~linearity ~uniqueness ~portability ~contention
    ~forkable ~yielding ~statefulness ~visibility ~staticity ~externality
    ~nullability ~separability =
  let open Levels in
  encode
    ~levels:
      [| level_of_areality areality;
         level_of_uniqueness_monadic uniqueness;
         level_of_linearity linearity;
         level_of_contention_monadic contention;
         level_of_portability portability;
         level_of_forkable forkable;
         level_of_yielding yielding;
         level_of_statefulness statefulness;
         level_of_visibility_monadic visibility;
         level_of_staticity_monadic staticity;
         level_of_externality externality;
         level_of_nullability nullability;
         level_of_separability separability
      |]

(* Canonical lattice constants used by ikinds. *)
let nonfloat_value : t =
  const_of_levels ~areality:Mode.Regionality.Const.max
    ~linearity:Mode.Linearity.Const.max ~uniqueness:Mode.Uniqueness.Const.Unique
    ~portability:Mode.Portability.Const.max
    ~contention:Mode.Contention.Const.Uncontended
    ~forkable:Mode.Forkable.Const.max ~yielding:Mode.Yielding.Const.max
    ~statefulness:Mode.Statefulness.Const.max
    ~visibility:Mode.Visibility.Const.Read_write
    ~staticity:Mode.Staticity.Static ~externality:Jkind_axis.Externality.max
    ~nullability:Jkind_axis.Nullability.Non_null
    ~separability:Jkind_axis.Separability.Non_float

let immutable_data : t =
  const_of_levels ~areality:Mode.Regionality.Const.max
    ~linearity:Mode.Linearity.Const.min ~uniqueness:Mode.Uniqueness.Const.Unique
    ~portability:Mode.Portability.Const.min
    ~contention:Mode.Contention.Const.Contended
    ~forkable:Mode.Forkable.Const.min ~yielding:Mode.Yielding.Const.min
    ~statefulness:Mode.Statefulness.Const.min
    ~visibility:Mode.Visibility.Const.Immutable ~staticity:Mode.Staticity.Static
    ~externality:Jkind_axis.Externality.max
    ~nullability:Jkind_axis.Nullability.Non_null
    ~separability:Jkind_axis.Separability.Non_float

let mutable_data : t =
  const_of_levels ~areality:Mode.Regionality.Const.max
    ~linearity:Mode.Linearity.Const.min ~uniqueness:Mode.Uniqueness.Const.Unique
    ~portability:Mode.Portability.Const.min
    ~contention:Mode.Contention.Const.Uncontended
    ~forkable:Mode.Forkable.Const.min ~yielding:Mode.Yielding.Const.min
    ~statefulness:Mode.Statefulness.Const.min
    ~visibility:Mode.Visibility.Const.Read_write
    ~staticity:Mode.Staticity.Static ~externality:Jkind_axis.Externality.max
    ~nullability:Jkind_axis.Nullability.Non_null
    ~separability:Jkind_axis.Separability.Non_float

let value : t =
  const_of_levels ~areality:Mode.Regionality.Const.max
    ~linearity:Mode.Linearity.Const.max ~uniqueness:Mode.Uniqueness.Const.Unique
    ~portability:Mode.Portability.Const.max
    ~contention:Mode.Contention.Const.Uncontended
    ~forkable:Mode.Forkable.Const.min ~yielding:Mode.Yielding.Const.max
    ~statefulness:Mode.Statefulness.Const.max
    ~visibility:Mode.Visibility.Const.Read_write
    ~staticity:Mode.Staticity.Static ~externality:Jkind_axis.Externality.max
    ~nullability:Jkind_axis.Nullability.Non_null
    ~separability:Jkind_axis.Separability.Separable

let arrow : t =
  const_of_levels ~areality:Mode.Regionality.Const.max
    ~linearity:Mode.Linearity.Const.max
    ~uniqueness:Mode.Uniqueness.Const.Aliased
    ~portability:Mode.Portability.Const.max
    ~contention:Mode.Contention.Const.Contended
    ~forkable:Mode.Forkable.Const.max ~yielding:Mode.Yielding.Const.max
    ~statefulness:Mode.Statefulness.Const.max
    ~visibility:Mode.Visibility.Const.Immutable ~staticity:Mode.Staticity.Static
    ~externality:Jkind_axis.Externality.max
    ~nullability:Jkind_axis.Nullability.Non_null
    ~separability:Jkind_axis.Separability.Non_float

let immediate : t =
  const_of_levels ~areality:Mode.Regionality.Const.min
    ~linearity:Mode.Linearity.Const.min
    ~uniqueness:Mode.Uniqueness.Const.Aliased
    ~portability:Mode.Portability.Const.min
    ~contention:Mode.Contention.Const.Contended
    ~forkable:Mode.Forkable.Const.min ~yielding:Mode.Yielding.Const.min
    ~statefulness:Mode.Statefulness.Const.min
    ~visibility:Mode.Visibility.Const.Immutable ~staticity:Mode.Staticity.Static
    ~externality:Jkind_axis.Externality.min
    ~nullability:Jkind_axis.Nullability.Non_null
    ~separability:Jkind_axis.Separability.Non_float

let object_legacy : t =
  let ({ linearity; areality; portability; forkable; yielding; statefulness }
        : Mode.Value.Comonadic.Const.t) =
    Mode.Value.Comonadic.Const.legacy
  in
  const_of_levels ~linearity ~areality ~uniqueness:Mode.Uniqueness.Const.Unique
    ~portability ~contention:Mode.Contention.Const.Uncontended ~forkable
    ~yielding ~statefulness ~visibility:Mode.Visibility.Const.Read_write
    ~staticity:Mode.Staticity.Static ~externality:Jkind_axis.Externality.max
    ~nullability:Jkind_axis.Nullability.Non_null
    ~separability:Jkind_axis.Separability.Non_float

let axis_number_to_axis_packed (axis_number : int) : Jkind_axis.Axis.packed =
  if axis_number < 0 || axis_number >= Array.length axis_by_number
  then failwith "axis_number_to_axis_packed: invalid axis number"
  else axis_by_number.(axis_number)
