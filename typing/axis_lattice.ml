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

module Prefix_ones_bitfield = Misc.Prefix_ones_bitfield
module Modal_bit_layout = Misc.Modal_bit_layout

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

let axis_by_number = Array.of_list Jkind_axis.Axis.all

let modal_slots =
  Mode.Crossing.Axis.all
  |> List.map (fun (Mode.Crossing.Axis.P axis) -> Mode.Crossing.Axis.slot axis)
  |> Array.of_list

let axis_sizes =
  let modal_sizes =
    Array.map (fun slot -> Modal_bit_layout.width slot + 1) modal_slots
  in
  Array.append modal_sizes [| 3; 2; 3 |]

let num_axes = Array.length axis_sizes

(* widths[i] = 2 for size-3 axes, 1 for size-2 *)
let widths =
  Array.mapi
    (fun i size ->
      if i < Array.length modal_slots
      then Modal_bit_layout.width modal_slots.(i)
      else Prefix_ones_bitfield.width_of_size size)
    axis_sizes

let offsets =
  Array.append
    (Array.map Modal_bit_layout.offset modal_slots)
    [| Modal_bit_layout.modal_width;
       Modal_bit_layout.modal_width + widths.(10);
       Modal_bit_layout.modal_width + widths.(10) + widths.(11)
    |]

let lo_mask =
  Array.mapi
    (fun i offset ->
      if i < Array.length modal_slots
      then
        Prefix_ones_bitfield.low_mask
          ~offset:(Modal_bit_layout.offset modal_slots.(i))
      else Prefix_ones_bitfield.low_mask ~offset)
    offsets

let axis_mask =
  Array.mapi
    (fun i off ->
      if i < Array.length modal_slots
      then
        Prefix_ones_bitfield.mask
          ~offset:off
          ~width:(Modal_bit_layout.width modal_slots.(i))
      else Prefix_ones_bitfield.mask ~offset:off ~width:widths.(i))
    offsets

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

let get_axis (v : t) ~axis:i : int =
  Prefix_ones_bitfield.get_level ~offset:offsets.(i) ~width:widths.(i) v

let set_axis (v : t) ~axis:i ~level:lev : t =
  Prefix_ones_bitfield.set_level ~offset:offsets.(i) ~width:widths.(i) lev v

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

let co_sub (a : t) (b : t) : t = Prefix_ones_bitfield.co_sub ~lows a b

(* Build a mask from a set of relevant axes. *)
let of_axis_set (set : Jkind_axis.Axis_set.t) : t =
  let set : int = Obj.magic set in
  let lo =
    (set land 0x001)
    lor ((set land 0x00E) lsl 1)
    lor ((set land 0x010) lsl 2)
    lor ((set land 0x0E0) lsl 3)
    lor ((set land 0x100) lsl 4)
    lor ((set land 0x600) lsl 5)
    lor ((set land 0x1800) lsl 6)
  in
  lo lor ((lo land 0x49451) lsl 1)

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
          (Mode.Modality.Per_axis.is_constant axis_for_modality
             modality_on_axis)
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

let areality (x : t) : Mode.Regionality.Const.t =
  Levels.areality_of_level (get_axis x ~axis:0)

let uniqueness (x : t) : Mode.Uniqueness.Const.t =
  Levels.uniqueness_of_level_monadic (get_axis x ~axis:1)

let linearity (x : t) : Mode.Linearity.Const.t =
  Levels.linearity_of_level (get_axis x ~axis:2)

let contention (x : t) : Mode.Contention.Const.t =
  Levels.contention_of_level_monadic (get_axis x ~axis:3)

let portability (x : t) : Mode.Portability.Const.t =
  Levels.portability_of_level (get_axis x ~axis:4)

let forkable (x : t) : Mode.Forkable.Const.t =
  Levels.forkable_of_level (get_axis x ~axis:5)

let yielding (x : t) : Mode.Yielding.Const.t =
  Levels.yielding_of_level (get_axis x ~axis:6)

let statefulness (x : t) : Mode.Statefulness.Const.t =
  Levels.statefulness_of_level (get_axis x ~axis:7)

let visibility (x : t) : Mode.Visibility.Const.t =
  Levels.visibility_of_level_monadic (get_axis x ~axis:8)

let staticity (x : t) : Mode.Staticity.const =
  Levels.staticity_of_level_monadic (get_axis x ~axis:9)

let externality (x : t) : Jkind_axis.Externality.t =
  Levels.externality_of_level (get_axis x ~axis:10)

let nullability (x : t) : Jkind_axis.Nullability.t =
  Levels.nullability_of_level (get_axis x ~axis:11)

let separability (x : t) : Jkind_axis.Separability.t =
  Levels.separability_of_level (get_axis x ~axis:12)

let set_areality (a : Mode.Regionality.Const.t) (x : t) : t =
  set_axis x ~axis:0 ~level:(Levels.level_of_areality a)

let set_uniqueness (u : Mode.Uniqueness.Const.t) (x : t) : t =
  set_axis x ~axis:1 ~level:(Levels.level_of_uniqueness_monadic u)

let set_linearity (l : Mode.Linearity.Const.t) (x : t) : t =
  set_axis x ~axis:2 ~level:(Levels.level_of_linearity l)

let set_contention (c : Mode.Contention.Const.t) (x : t) : t =
  set_axis x ~axis:3 ~level:(Levels.level_of_contention_monadic c)

let set_portability (p : Mode.Portability.Const.t) (x : t) : t =
  set_axis x ~axis:4 ~level:(Levels.level_of_portability p)

let set_forkable (f : Mode.Forkable.Const.t) (x : t) : t =
  set_axis x ~axis:5 ~level:(Levels.level_of_forkable f)

let set_yielding (y : Mode.Yielding.Const.t) (x : t) : t =
  set_axis x ~axis:6 ~level:(Levels.level_of_yielding y)

let set_statefulness (s : Mode.Statefulness.Const.t) (x : t) : t =
  set_axis x ~axis:7 ~level:(Levels.level_of_statefulness s)

let set_visibility (v : Mode.Visibility.Const.t) (x : t) : t =
  set_axis x ~axis:8 ~level:(Levels.level_of_visibility_monadic v)

let set_staticity (s : Mode.Staticity.const) (x : t) : t =
  set_axis x ~axis:9 ~level:(Levels.level_of_staticity_monadic s)

let set_externality (e : Jkind_axis.Externality.t) (x : t) : t =
  set_axis x ~axis:10 ~level:(Levels.level_of_externality e)

let set_nullability (n : Jkind_axis.Nullability.t) (x : t) : t =
  set_axis x ~axis:11 ~level:(Levels.level_of_nullability n)

let set_separability (s : Jkind_axis.Separability.t) (x : t) : t =
  set_axis x ~axis:12 ~level:(Levels.level_of_separability s)

let to_mode_crossing (x : t) : Mode.Crossing.t =
  let open Mode.Crossing in
  let monadic =
    Monadic.create
      ~uniqueness:
        (Monadic.Atom.Modality
           (Mode.Modality.Monadic.Atom.Join_const
              (uniqueness x)))
      ~contention:
        (Monadic.Atom.Modality
           (Mode.Modality.Monadic.Atom.Join_const
              (contention x)))
      ~visibility:
        (Monadic.Atom.Modality
           (Mode.Modality.Monadic.Atom.Join_const
              (visibility x)))
      ~staticity:
        (Monadic.Atom.Modality
           (Mode.Modality.Monadic.Atom.Join_const
              (staticity x)))
  in
  let comonadic =
    Comonadic.create
      ~regionality:
        (Comonadic.Atom.Modality
           (Mode.Modality.Comonadic.Atom.Meet_const
              (areality x)))
      ~linearity:
        (Comonadic.Atom.Modality
           (Mode.Modality.Comonadic.Atom.Meet_const
              (linearity x)))
      ~portability:
        (Comonadic.Atom.Modality
           (Mode.Modality.Comonadic.Atom.Meet_const
              (portability x)))
      ~forkable:
        (Comonadic.Atom.Modality
           (Mode.Modality.Comonadic.Atom.Meet_const
              (forkable x)))
      ~yielding:
        (Comonadic.Atom.Modality
           (Mode.Modality.Comonadic.Atom.Meet_const
              (yielding x)))
      ~statefulness:
        (Comonadic.Atom.Modality
           (Mode.Modality.Comonadic.Atom.Meet_const
              (statefulness x)))
  in
  pack ~monadic ~comonadic

let create ~areality ~linearity ~uniqueness ~portability ~contention
    ~forkable ~yielding ~statefulness ~visibility ~staticity ~externality
    ~nullability ~separability =
  bot
  |> set_areality areality
  |> set_uniqueness uniqueness
  |> set_linearity linearity
  |> set_contention contention
  |> set_portability portability
  |> set_forkable forkable
  |> set_yielding yielding
  |> set_statefulness statefulness
  |> set_visibility visibility
  |> set_staticity staticity
  |> set_externality externality
  |> set_nullability nullability
  |> set_separability separability

(* Canonical lattice constants used by ikinds. *)
let nonfloat_value : t =
  create ~areality:Mode.Regionality.Const.max
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
  create ~areality:Mode.Regionality.Const.max
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
  create ~areality:Mode.Regionality.Const.max
    ~linearity:Mode.Linearity.Const.min ~uniqueness:Mode.Uniqueness.Const.Unique
    ~portability:Mode.Portability.Const.min
    ~contention:Mode.Contention.Const.Uncontended
    ~forkable:Mode.Forkable.Const.min ~yielding:Mode.Yielding.Const.min
    ~statefulness:Mode.Statefulness.Const.min
    ~visibility:Mode.Visibility.Const.Read_write
    ~staticity:Mode.Staticity.Static ~externality:Jkind_axis.Externality.max
    ~nullability:Jkind_axis.Nullability.Non_null
    ~separability:Jkind_axis.Separability.Non_float

let sync_data : t =
  create ~areality:Mode.Regionality.Const.max
    ~linearity:Mode.Linearity.Const.min ~uniqueness:Mode.Uniqueness.Const.Unique
    ~portability:Mode.Portability.Const.min
    ~contention:Mode.Contention.Const.Contended
    ~forkable:Mode.Forkable.Const.min ~yielding:Mode.Yielding.Const.min
    ~statefulness:Mode.Statefulness.Const.min
    ~visibility:Mode.Visibility.Const.Read_write
    ~staticity:Mode.Staticity.Static ~externality:Jkind_axis.Externality.max
    ~nullability:Jkind_axis.Nullability.Non_null
    ~separability:Jkind_axis.Separability.Non_float

let value : t =
  create ~areality:Mode.Regionality.Const.max
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
  create ~areality:Mode.Regionality.Const.max
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
  create ~areality:Mode.Regionality.Const.min
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
        : Mode.Value.Comonadic.repr) =
    Mode.Value.Comonadic.decode Mode.Value.Comonadic.Const.legacy
  in
  create ~linearity ~areality
    ~uniqueness:Mode.Uniqueness.Const.Aliased
    ~portability ~contention:Mode.Contention.Const.Uncontended ~forkable
    ~yielding ~statefulness ~visibility:Mode.Visibility.Const.Read_write
    ~staticity:Mode.Staticity.Static ~externality:Jkind_axis.Externality.max
    ~nullability:Jkind_axis.Nullability.Non_null
    ~separability:Jkind_axis.Separability.Non_float

let axis_number_to_axis_packed (axis_number : int) : Jkind_axis.Axis.packed =
  if axis_number < 0 || axis_number >= Array.length axis_by_number
  then failwith "axis_number_to_axis_packed: invalid axis number"
  else axis_by_number.(axis_number)
