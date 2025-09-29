(* Axis lattice bits: efficient bitfield encoding of jkind axes.

   This module packs 11 axes into a 17-bit integer, where each axis can have
   2 or 3 possible values (levels). The axes are indexed 0-10 and their values
   are ordered from most restrictive (0) to least restrictive (max).

   Axis layout (index, name, values from level 0 to max):
   0. Areality (Regionality): Global -> Regional -> Local
   1. Linearity: Many -> Once
   2. Uniqueness (monadic): Aliased -> Unique
   3. Portability: Portable -> Nonportable
   4. Contention (monadic): Contended -> Shared -> Uncontended
   5. Yielding: Unyielding -> Yielding
   6. Statefulness: Stateless -> Observing -> Stateful
   7. Visibility (monadic): Immutable -> Read -> Read_write
   8. Externality: External -> External64 -> Internal
   9. Nullability: Non_null -> Maybe_null
   10. Separability: Non_float -> Separable -> Maybe_separable

   Axes 0-7 are modal axes (affect mode-crossing).
   Axes 8-10 are non-modal axes (externality and shallow axes).
   Axes 9-10 are "shallow" axes (nullability and separability) that are
   sometimes excluded from masking operations.

   Each 3-valued axis uses 2 bits, each 2-valued axis uses 1 bit.
   Total: 2*7 + 1*4 = 17 bits, fitting in an OCaml immediate integer. *)

let axis_sizes = [| 3; 2; 2; 2; 3; 2; 3; 3; 3; 2; 3 |]
let num_axes = 11

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
    off.(i) <- !a; a := !a + widths.(i)
  done; off

(* 1 if axis i has a high bit (i.e. width=2), else 0 *)
let has_hi = Array.init num_axes (fun i -> (widths.(i) lsr 1) land 1)

let lo_mask   = Array.init num_axes (fun i -> 1 lsl offsets.(i))
let hi_mask   = Array.init num_axes (fun i ->
                  if has_hi.(i) = 1 then (1 lsl (offsets.(i) + 1)) else 0)
let axis_mask = Array.init num_axes (fun i -> lo_mask.(i) lor hi_mask.(i))

(* OR of all low bits (for size-2 axes that’s their only bit).
   For this layout: 0xD5BD. *)
let lows = Array.fold_left (lor) 0 lo_mask

type t = int
let bot : t = 0
(* For this layout top happens to be all 17 bits set: 0x1_FFFF. *)
let top : t = Array.fold_left (lor) 0 axis_mask

let join (a:t) (b:t) : t = a lor b
let meet (a:t) (b:t) : t = a land b
let leq  (a:t) (b:t) : bool = (a land b) = a
let equal (a:t) (b:t) : bool = a = b
let hash = Hashtbl.hash

(* Branchless get: for 3-ary axes level = lo + hi (00→0, 01→1, 11→2).
    For 2-ary axes hi=0 (masked by has_hi). *)
let get_axis (v:t) ~axis:i : int =
  let off = offsets.(i) in
  let lo  = (v lsr off) land 1 in
  let hi  = ((v lsr (off + 1)) land has_hi.(i)) in
  lo + hi

(* Branchless set:
    low_bit  = (lev | (lev >> 1)) & 1  (0→0, 1→1, 2→1)
    high_bit = (lev >> 1) & has_hi
      (0→0, 1→0, 2→1; zeroed for 1-bit axes)
    No range checks—caller keeps lev in-range. *)
let set_axis (v:t) ~axis:i ~level:lev : t =
  let off = offsets.(i) in
  let cleared = v land (lnot axis_mask.(i)) in
  let lo  = (lev lor (lev lsr 1)) land 1 in
  let hi  = (lev lsr 1) land has_hi.(i) in
  cleared lor (lo lsl off) lor (hi lsl (off + 1))

let encode ~levels : t =
  let v = ref 0 in
  for i = 0 to num_axes - 1 do
    v := set_axis !v ~axis:i ~level:levels.(i)
  done;
  !v

let decode (v:t) : int array =
  Array.init num_axes (fun i -> get_axis v ~axis:i)

let non_bot_axes (v : t) : int list =
  let rec loop i acc =
    if i = num_axes then List.rev acc
    else
      let acc' = if (v land axis_mask.(i)) <> 0 then i :: acc else acc in
      loop (i + 1) acc'
  in
  loop 0 []

let pp (v:t) : string =
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
let co_sub (a:t) (b:t) : t =
  let r = a land (lnot b) in
  r lor ((r lsr 1) land lnot_lsr_1_lows)

(* Individual axis constants: each axis at a specific non-bot level *)

(* Areality axis: 0=Global, 1=Regional, 2=Local *)
let areality_global    : t = set_axis bot ~axis:0 ~level:0
let areality_regional  : t = set_axis bot ~axis:0 ~level:1
let areality_local     : t = set_axis bot ~axis:0 ~level:2

(* Linearity axis: 0=Many, 1=Once *)
let linearity_many : t = set_axis bot ~axis:1 ~level:0
let linearity_once : t = set_axis bot ~axis:1 ~level:1

(* Uniqueness axis (monadic): 0=Aliased, 1=Unique *)
let uniqueness_aliased : t = set_axis bot ~axis:2 ~level:0
let uniqueness_unique  : t = set_axis bot ~axis:2 ~level:1

(* Portability axis: 0=Portable, 1=Nonportable *)
let portability_portable    : t = set_axis bot ~axis:3 ~level:0
let portability_nonportable : t = set_axis bot ~axis:3 ~level:1

(* Contention axis (monadic): 0=Contended, 1=Shared, 2=Uncontended *)
let contention_contended   : t = set_axis bot ~axis:4 ~level:0
let contention_shared      : t = set_axis bot ~axis:4 ~level:1
let contention_uncontended : t = set_axis bot ~axis:4 ~level:2

(* Yielding axis: 0=Unyielding, 1=Yielding *)
let yielding_unyielding : t = set_axis bot ~axis:5 ~level:0
let yielding_yielding   : t = set_axis bot ~axis:5 ~level:1

(* Statefulness axis: 0=Stateless, 1=Observing, 2=Stateful *)
let statefulness_stateless : t = set_axis bot ~axis:6 ~level:0
let statefulness_observing : t = set_axis bot ~axis:6 ~level:1
let statefulness_stateful  : t = set_axis bot ~axis:6 ~level:2

(* Visibility axis (monadic): 0=Immutable, 1=Read, 2=Read_write *)
let visibility_immutable  : t = set_axis bot ~axis:7 ~level:0
let visibility_read       : t = set_axis bot ~axis:7 ~level:1
let visibility_read_write : t = set_axis bot ~axis:7 ~level:2

(* Externality axis: 0=External, 1=External64, 2=Internal *)
let externality_external   : t = set_axis bot ~axis:8 ~level:0
let externality_external64 : t = set_axis bot ~axis:8 ~level:1
let externality_internal   : t = set_axis bot ~axis:8 ~level:2

(* Nullability axis: 0=Non_null, 1=Maybe_null *)
let nullability_non_null   : t = set_axis bot ~axis:9 ~level:0
let nullability_maybe_null : t = set_axis bot ~axis:9 ~level:1

(* Separability axis: 0=Non_float, 1=Separable, 2=Maybe_separable *)
let separability_non_float       : t = set_axis bot ~axis:10 ~level:0
let separability_separable       : t = set_axis bot ~axis:10 ~level:1
let separability_maybe_separable : t = set_axis bot ~axis:10 ~level:2

(* Build a mask from a set of relevant axes. *)
let of_axis_set (set : Jkind_axis.Axis_set.t) : t =
  let levels = Array.make num_axes 0 in
  let open Jkind_axis in
  let set_idx_by_name (name : string) =
    let top i = axis_sizes.(i) - 1 in
    let idx =
      match name with
      | "areality" -> Some 0
      | "linearity" -> Some 1
      | "uniqueness" -> Some 2
      | "portability" -> Some 3
      | "contention" -> Some 4
      | "yielding" -> Some 5
      | "statefulness" -> Some 6
      | "visibility" -> Some 7
      | "externality" -> Some 8
      | "nullability" -> Some 9
      | "separability" -> Some 10
      | _ -> None
    in
    match idx with None -> () | Some i -> levels.(i) <- top i
  in
  Axis_set.to_seq set
  |> Seq.iter (fun (Axis.Pack ax) -> set_idx_by_name (Axis.name ax));
  encode ~levels

(* IK-only: compute relevant axes of a constant modality, mirroring
   Jkind.relevant_axes_of_modality. *)
let relevant_axes_of_modality
    ~(relevant_for_shallow : [`Relevant | `Irrelevant])
    (modality : Mode.Modality.Const.t) : Jkind_axis.Axis_set.t =
  Jkind_axis.Axis_set.create ~f:(fun ~axis:(Jkind_axis.Axis.Pack axis) ->
      match axis with
      | Jkind_axis.Axis.Modal axis ->
        let (Mode.Modality.Axis.P axis_for_modality) =
          Mode.Crossing.Axis.(P axis |> to_modality)
        in
        let modality_on_axis =
          Mode.Modality.Const.proj axis_for_modality modality
        in
        not
          (Mode.Modality.Per_axis.is_constant
             axis_for_modality modality_on_axis)
      | Jkind_axis.Axis.Nonmodal Jkind_axis.Axis.Nonmodal.Externality -> true
      | Jkind_axis.Axis.Nonmodal Jkind_axis.Axis.Nonmodal.Nullability -> (
        match relevant_for_shallow with
        | `Relevant -> true
        | `Irrelevant -> false)
      | Jkind_axis.Axis.Nonmodal Jkind_axis.Axis.Nonmodal.Separability -> (
        match relevant_for_shallow with
        | `Relevant -> true
        | `Irrelevant -> false))

(* Mask that excludes the shallow axes (nullability and separability). *)
let mask_shallow : t =
  co_sub top (join axis_mask.(9) axis_mask.(10))

(* Directly produce an axis-lattice mask from a constant modality. *)
let mask_of_modality ~(relevant_for_shallow : [`Relevant | `Irrelevant])
    (modality : Mode.Modality.Const.t) : t =
  relevant_axes_of_modality ~relevant_for_shallow modality |> of_axis_set

(* Conversion between Types.Jkind_mod_bounds.t and Axis_lattice_bits.t *)

let level_of_areality (a : Mode.Regionality.Const.t) : int =
  match a with
  | Mode.Regionality.Const.Global -> 0
  | Mode.Regionality.Const.Regional -> 1
  | Mode.Regionality.Const.Local -> 2

let areality_of_level = function
  | 0 -> Mode.Regionality.Const.Global
  | 1 -> Mode.Regionality.Const.Regional
  | 2 -> Mode.Regionality.Const.Local
  | _ -> invalid_arg "Axis_lattice_bits.areality_of_level"

let level_of_linearity (x : Mode.Linearity.Const.t) : int =
  match x with Mode.Linearity.Const.Many -> 0 | Mode.Linearity.Const.Once -> 1

let linearity_of_level = function
  | 0 -> Mode.Linearity.Const.Many
  | 1 -> Mode.Linearity.Const.Once
  | _ -> invalid_arg "Axis_lattice_bits.linearity_of_level"

let level_of_uniqueness_monadic (x : Mode.Uniqueness.Const.t) : int =
  match x with
  | Mode.Uniqueness.Const.Unique -> 1
  | Mode.Uniqueness.Const.Aliased -> 0

let uniqueness_of_level_monadic = function
  | 0 -> Mode.Uniqueness.Const.Aliased
  | 1 -> Mode.Uniqueness.Const.Unique
  | _ -> invalid_arg "Axis_lattice_bits.uniqueness_of_level_monadic"

let level_of_portability (x : Mode.Portability.Const.t) : int =
  match x with
  | Mode.Portability.Const.Portable -> 0
  | Mode.Portability.Const.Nonportable -> 1

let portability_of_level = function
  | 0 -> Mode.Portability.Const.Portable
  | 1 -> Mode.Portability.Const.Nonportable
  | _ -> invalid_arg "Axis_lattice_bits.portability_of_level"

let level_of_contention_monadic (x : Mode.Contention.Const.t) : int =
  match x with
  | Mode.Contention.Const.Contended -> 0
  | Mode.Contention.Const.Shared -> 1
  | Mode.Contention.Const.Uncontended -> 2

let contention_of_level_monadic = function
  | 0 -> Mode.Contention.Const.Contended
  | 1 -> Mode.Contention.Const.Shared
  | 2 -> Mode.Contention.Const.Uncontended
  | _ -> invalid_arg "Axis_lattice_bits.contention_of_level_monadic"

let level_of_yielding (x : Mode.Yielding.Const.t) : int =
  match x with
  | Mode.Yielding.Const.Unyielding -> 0
  | Mode.Yielding.Const.Yielding -> 1

let yielding_of_level = function
  | 0 -> Mode.Yielding.Const.Unyielding
  | 1 -> Mode.Yielding.Const.Yielding
  | _ -> invalid_arg "Axis_lattice_bits.yielding_of_level"

let level_of_statefulness (x : Mode.Statefulness.Const.t) : int =
  match x with
  | Mode.Statefulness.Const.Stateless -> 0
  | Mode.Statefulness.Const.Observing -> 1
  | Mode.Statefulness.Const.Stateful -> 2

let statefulness_of_level = function
  | 0 -> Mode.Statefulness.Const.Stateless
  | 1 -> Mode.Statefulness.Const.Observing
  | 2 -> Mode.Statefulness.Const.Stateful
  | _ -> invalid_arg "Axis_lattice_bits.statefulness_of_level"

let level_of_visibility_monadic (x : Mode.Visibility.Const.t) : int =
  match x with
  | Mode.Visibility.Const.Immutable -> 0
  | Mode.Visibility.Const.Read -> 1
  | Mode.Visibility.Const.Read_write -> 2

let visibility_of_level_monadic = function
  | 0 -> Mode.Visibility.Const.Immutable
  | 1 -> Mode.Visibility.Const.Read
  | 2 -> Mode.Visibility.Const.Read_write
  | _ -> invalid_arg "Axis_lattice_bits.visibility_of_level_monadic"

let level_of_externality (x : Jkind_axis.Externality.t) : int =
  match x with
  | External -> 0
  | External64 -> 1
  | Internal -> 2

let externality_of_level = function
  | 0 -> Jkind_axis.Externality.External
  | 1 -> Jkind_axis.Externality.External64
  | 2 -> Jkind_axis.Externality.Internal
  | _ -> invalid_arg "Axis_lattice_bits.externality_of_level"

let level_of_nullability (x : Jkind_axis.Nullability.t) : int =
  match x with Non_null -> 0 | Maybe_null -> 1

let nullability_of_level = function
  | 0 -> Jkind_axis.Nullability.Non_null
  | 1 -> Jkind_axis.Nullability.Maybe_null
  | _ -> invalid_arg "Axis_lattice_bits.nullability_of_level"

let level_of_separability (x : Jkind_axis.Separability.t) : int =
  match x with
  | Non_float -> 0
  | Separable -> 1
  | Maybe_separable -> 2

let separability_of_level = function
  | 0 -> Jkind_axis.Separability.Non_float
  | 1 -> Jkind_axis.Separability.Separable
  | 2 -> Jkind_axis.Separability.Maybe_separable
  | _ -> invalid_arg "Axis_lattice_bits.separability_of_level"

let crossing_of_constants ~areality ~linearity ~uniqueness ~portability
    ~contention ~yielding ~statefulness ~visibility : Mode.Crossing.t =
  let open Mode.Crossing in
  let monadic =
    Monadic.create
      ~uniqueness:
        (Monadic.Atom.Modality
           (Mode.Modality.Monadic.Atom.Join_with uniqueness))
      ~contention:
        (Monadic.Atom.Modality
           (Mode.Modality.Monadic.Atom.Join_with contention))
      ~visibility:
        (Monadic.Atom.Modality
           (Mode.Modality.Monadic.Atom.Join_with visibility))
  in
  let comonadic =
    Comonadic.create
      ~regionality:
        (Comonadic.Atom.Modality
           (Mode.Modality.Comonadic.Atom.Meet_with areality))
      ~linearity:
        (Comonadic.Atom.Modality
           (Mode.Modality.Comonadic.Atom.Meet_with linearity))
      ~portability:
        (Comonadic.Atom.Modality
           (Mode.Modality.Comonadic.Atom.Meet_with portability))
      ~yielding:
        (Comonadic.Atom.Modality
           (Mode.Modality.Comonadic.Atom.Meet_with yielding))
      ~statefulness:
        (Comonadic.Atom.Modality
           (Mode.Modality.Comonadic.Atom.Meet_with statefulness))
  in
  { monadic; comonadic }

let of_mod_bounds (mb : Types.Jkind_mod_bounds.t) : t =
  let open Types.Jkind_mod_bounds in
  let levels =
    [| level_of_areality (areality_const mb);
       level_of_linearity (linearity_const mb);
       level_of_uniqueness_monadic (uniqueness_const mb);
       level_of_portability (portability_const mb);
       level_of_contention_monadic (contention_const mb);
       level_of_yielding (yielding_const mb);
       level_of_statefulness (statefulness_const mb);
       level_of_visibility_monadic (visibility_const mb);
       level_of_externality (externality mb);
       level_of_nullability (nullability mb);
       level_of_separability (separability mb)
    |]
  in
  encode ~levels

let to_mod_bounds (x : t) : Types.Jkind_mod_bounds.t =
  let lv = decode x in
  let areality = areality_of_level lv.(0) in
  let linearity = linearity_of_level lv.(1) in
  let uniqueness = uniqueness_of_level_monadic lv.(2) in
  let portability = portability_of_level lv.(3) in
  let contention = contention_of_level_monadic lv.(4) in
  let yielding = yielding_of_level lv.(5) in
  let statefulness = statefulness_of_level lv.(6) in
  let visibility = visibility_of_level_monadic lv.(7) in
  let externality = externality_of_level lv.(8) in
  let nullability = nullability_of_level lv.(9) in
  let separability = separability_of_level lv.(10) in
  let crossing =
    crossing_of_constants ~areality ~linearity ~uniqueness ~portability
      ~contention ~yielding ~statefulness ~visibility
  in
  Types.Jkind_mod_bounds.create crossing ~externality ~nullability
    ~separability

(* Canonical lattice constants used by ikinds. *)
let nonfloat_value : t =
  let crossing =
    crossing_of_constants ~areality:Mode.Regionality.Const.max
      ~linearity:Mode.Linearity.Const.max
      ~uniqueness:Mode.Uniqueness.Const.Unique
      ~portability:Mode.Portability.Const.max
      ~contention:Mode.Contention.Const.Uncontended
      ~yielding:Mode.Yielding.Const.max
      ~statefulness:Mode.Statefulness.Const.max
      ~visibility:Mode.Visibility.Const.Read_write
  in
  let mb =
    Types.Jkind_mod_bounds.create crossing
      ~externality:Jkind_axis.Externality.max
      ~nullability:Jkind_axis.Nullability.Non_null
      ~separability:Jkind_axis.Separability.Non_float
  in
  of_mod_bounds mb

let immutable_data : t =
  let crossing =
    crossing_of_constants ~areality:Mode.Regionality.Const.max
      ~linearity:Mode.Linearity.Const.min
      ~uniqueness:Mode.Uniqueness.Const.Unique
      ~portability:Mode.Portability.Const.min
      ~contention:Mode.Contention.Const.Contended
      ~yielding:Mode.Yielding.Const.min
      ~statefulness:Mode.Statefulness.Const.min
      ~visibility:Mode.Visibility.Const.Immutable
  in
  let mb =
    Types.Jkind_mod_bounds.create crossing
      ~externality:Jkind_axis.Externality.max
      ~nullability:Jkind_axis.Nullability.Non_null
      ~separability:Jkind_axis.Separability.Non_float
  in
  of_mod_bounds mb

let mutable_data : t =
  let crossing =
    crossing_of_constants ~areality:Mode.Regionality.Const.max
      ~linearity:Mode.Linearity.Const.min
      ~uniqueness:Mode.Uniqueness.Const.Unique
      ~portability:Mode.Portability.Const.min
      ~contention:Mode.Contention.Const.Uncontended
      ~yielding:Mode.Yielding.Const.min
      ~statefulness:Mode.Statefulness.Const.min
      ~visibility:Mode.Visibility.Const.Read_write
  in
  let mb =
    Types.Jkind_mod_bounds.create crossing
      ~externality:Jkind_axis.Externality.max
      ~nullability:Jkind_axis.Nullability.Non_null
      ~separability:Jkind_axis.Separability.Non_float
  in
  of_mod_bounds mb

let value : t =
  let crossing =
    crossing_of_constants ~areality:Mode.Regionality.Const.max
      ~linearity:Mode.Linearity.Const.max
      ~uniqueness:Mode.Uniqueness.Const.Unique
      ~portability:Mode.Portability.Const.max
      ~contention:Mode.Contention.Const.Uncontended
      ~yielding:Mode.Yielding.Const.max
      ~statefulness:Mode.Statefulness.Const.max
      ~visibility:Mode.Visibility.Const.Read_write
  in
  let mb =
    Types.Jkind_mod_bounds.create crossing
      ~externality:Jkind_axis.Externality.max
      ~nullability:Jkind_axis.Nullability.Non_null
      ~separability:Jkind_axis.Separability.Separable
  in
  of_mod_bounds mb

let arrow : t =
  let crossing =
    crossing_of_constants ~areality:Mode.Regionality.Const.max
      ~linearity:Mode.Linearity.Const.max
      ~uniqueness:Mode.Uniqueness.Const.Aliased
      ~portability:Mode.Portability.Const.max
      ~contention:Mode.Contention.Const.Contended
      ~yielding:Mode.Yielding.Const.max
      ~statefulness:Mode.Statefulness.Const.max
      ~visibility:Mode.Visibility.Const.Immutable
  in
  let mb =
    Types.Jkind_mod_bounds.create crossing
      ~externality:Jkind_axis.Externality.max
      ~nullability:Jkind_axis.Nullability.Non_null
      ~separability:Jkind_axis.Separability.Non_float
  in
  of_mod_bounds mb

let immediate : t =
  let crossing =
    crossing_of_constants ~areality:Mode.Regionality.Const.min
      ~linearity:Mode.Linearity.Const.min
      ~uniqueness:Mode.Uniqueness.Const.Aliased
      ~portability:Mode.Portability.Const.min
      ~contention:Mode.Contention.Const.Contended
      ~yielding:Mode.Yielding.Const.min
      ~statefulness:Mode.Statefulness.Const.min
      ~visibility:Mode.Visibility.Const.Immutable
  in
  let mb =
    Types.Jkind_mod_bounds.create crossing
      ~externality:Jkind_axis.Externality.min
      ~nullability:Jkind_axis.Nullability.Non_null
      ~separability:Jkind_axis.Separability.Non_float
  in
  of_mod_bounds mb

let object_legacy : t =
  let ({ linearity; areality; portability; yielding; statefulness }
        : Mode.Value.Comonadic.Const.t) =
    Mode.Value.Comonadic.Const.legacy
  in
  let uniqueness = Mode.Uniqueness.Const.Unique in
  let contention = Mode.Contention.Const.Uncontended in
  let visibility = Mode.Visibility.Const.Read_write in
  let crossing =
    crossing_of_constants ~linearity ~areality ~uniqueness ~portability
      ~contention ~yielding ~statefulness ~visibility
  in
  let mb =
    Types.Jkind_mod_bounds.create crossing
      ~externality:Jkind_axis.Externality.max
      ~nullability:Jkind_axis.Nullability.Non_null
      ~separability:Jkind_axis.Separability.Non_float
  in
  of_mod_bounds mb

let axis_number_to_axis_packed (axis_number : int) : Jkind_axis.Axis.packed =
  let open Mode.Crossing.Axis in
  match axis_number with
  | 0 -> Jkind_axis.Axis.Pack (Jkind_axis.Axis.Modal (Comonadic Areality))
  | 1 -> Jkind_axis.Axis.Pack (Jkind_axis.Axis.Modal (Comonadic Linearity))
  | 2 -> Jkind_axis.Axis.Pack (Jkind_axis.Axis.Modal (Monadic Uniqueness))
  | 3 -> Jkind_axis.Axis.Pack (Jkind_axis.Axis.Modal (Comonadic Portability))
  | 4 -> Jkind_axis.Axis.Pack (Jkind_axis.Axis.Modal (Monadic Contention))
  | 5 -> Jkind_axis.Axis.Pack (Jkind_axis.Axis.Modal (Comonadic Yielding))
  | 6 -> Jkind_axis.Axis.Pack (Jkind_axis.Axis.Modal (Comonadic Statefulness))
  | 7 -> Jkind_axis.Axis.Pack (Jkind_axis.Axis.Modal (Monadic Visibility))
  | 8 ->
    Jkind_axis.Axis.Pack
      (Jkind_axis.Axis.Nonmodal Jkind_axis.Axis.Nonmodal.Externality)
  | 9 ->
    Jkind_axis.Axis.Pack
      (Jkind_axis.Axis.Nonmodal Jkind_axis.Axis.Nonmodal.Nullability)
  | 10 ->
    Jkind_axis.Axis.Pack
      (Jkind_axis.Axis.Nonmodal Jkind_axis.Axis.Nonmodal.Separability)
  | _ -> failwith "axis_number_to_axis_packed: invalid axis number"
