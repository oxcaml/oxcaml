(* Axis lattice: efficient bitfield encoding of jkind axes.

   The canonical packing of jkinds axes lives in [Axis_bitfield]; this module
   builds the domain specific helpers (conversion to/from mode records,
   modality masks, etc.) on top of that backend. *)

module Bit = Axis_bitfield

let num_axes = Bit.num_axes

(* Axes in the correct order matching axis_index (NOT Jkind_axis.Axis.all).
   This is the order used by Axis_set.create and axis_index.
   DO NOT use Jkind_axis.Axis.all directly as it has a different order. *)
let all_axes_correct_order : Jkind_axis.Axis.packed list =
  let open Mode.Crossing.Axis in
  [ Jkind_axis.Axis.Pack (Jkind_axis.Axis.Modal (Comonadic Areality));
    Jkind_axis.Axis.Pack (Jkind_axis.Axis.Modal (Comonadic Linearity));
    Jkind_axis.Axis.Pack (Jkind_axis.Axis.Modal (Monadic Uniqueness));
    Jkind_axis.Axis.Pack (Jkind_axis.Axis.Modal (Comonadic Portability));
    Jkind_axis.Axis.Pack (Jkind_axis.Axis.Modal (Monadic Contention));
    Jkind_axis.Axis.Pack (Jkind_axis.Axis.Modal (Comonadic Yielding));
    Jkind_axis.Axis.Pack (Jkind_axis.Axis.Modal (Comonadic Statefulness));
    Jkind_axis.Axis.Pack (Jkind_axis.Axis.Modal (Monadic Visibility));
    Jkind_axis.Axis.Pack (Jkind_axis.Axis.Nonmodal Jkind_axis.Axis.Nonmodal.Externality);
    Jkind_axis.Axis.Pack (Jkind_axis.Axis.Nonmodal Jkind_axis.Axis.Nonmodal.Nullability);
    Jkind_axis.Axis.Pack (Jkind_axis.Axis.Nonmodal Jkind_axis.Axis.Nonmodal.Separability) ]

type t = Bit.t

let bot = Bit.bot
let top = Bit.top
let join = Bit.join
let meet = Bit.meet
let leq = Bit.leq
let equal = Bit.equal
let hash = Bit.hash
let co_sub = Bit.co_sub
let non_bot_axes = Bit.non_bot_axes
let to_string = Bit.to_string

(* Individual axis constants: each axis at a specific non-bot level *)
(* Areality axis: 0=Global, 1=Regional, 2=Local *)
let areality_global : t = Bit.areality_global
let areality_regional : t = Bit.areality_regional
let areality_local : t = Bit.areality_local

(* Linearity axis: 0=Many, 1=Once *)
let linearity_many : t = Bit.linearity_many
let linearity_once : t = Bit.linearity_once

(* Uniqueness axis (monadic): 0=Aliased, 1=Unique *)
let uniqueness_aliased : t = Bit.uniqueness_aliased
let uniqueness_unique : t = Bit.uniqueness_unique

(* Portability axis: 0=Portable, 1=Nonportable *)
let portability_portable : t = Bit.portability_portable
let portability_nonportable : t = Bit.portability_nonportable

(* Contention axis (monadic): 0=Contended, 1=Shared, 2=Uncontended *)
let contention_contended : t = Bit.contention_contended
let contention_shared : t = Bit.contention_shared
let contention_uncontended : t = Bit.contention_uncontended

(* Yielding axis: 0=Unyielding, 1=Yielding *)
let yielding_unyielding : t = Bit.yielding_unyielding
let yielding_yielding : t = Bit.yielding_yielding

(* Statefulness axis: 0=Stateless, 1=Observing, 2=Stateful *)
let statefulness_stateless : t = Bit.statefulness_stateless
let statefulness_observing : t = Bit.statefulness_observing
let statefulness_stateful : t = Bit.statefulness_stateful

(* Visibility axis (monadic): 0=Immutable, 1=Read, 2=Read_write *)
let visibility_immutable : t = Bit.visibility_immutable
let visibility_read : t = Bit.visibility_read
let visibility_read_write : t = Bit.visibility_read_write

(* Externality axis: 0=External, 1=External64, 2=Internal *)
let externality_external : t = Bit.externality_external
let externality_external64 : t = Bit.externality_external64
let externality_internal : t = Bit.externality_internal

(* Nullability axis: 0=Non_null, 1=Maybe_null *)
let nullability_non_null : t = Bit.nullability_non_null
let nullability_maybe_null : t = Bit.nullability_maybe_null

(* Separability axis: 0=Non_float, 1=Separable, 2=Maybe_separable *)
let separability_non_float : t = Bit.separability_non_float
let separability_separable : t = Bit.separability_separable
let separability_maybe_separable : t = Bit.separability_maybe_separable

(* Build a mask from a set of relevant axes. *)
let of_axis_set (set : Jkind_axis.Axis_set.t) : t =
  let levels = Array.make num_axes 0 in
  let open Jkind_axis in
  (* Iterate in the correct axis_index order, not Axis.all order *)
  List.iteri (fun i (Axis.Pack ax) ->
    if Axis_set.mem set ax then
      levels.(i) <- Bit.max_level ~axis:i
  ) all_axes_correct_order;
  Bit.encode ~levels

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
let mask_shallow : t = Bit.mask_shallow

(* Directly produce an axis-lattice mask from a constant modality. *)
let mask_of_modality ~(relevant_for_shallow : [`Relevant | `Irrelevant])
    (modality : Mode.Modality.Const.t) : t =
  relevant_axes_of_modality ~relevant_for_shallow modality |> of_axis_set

(* Conversion between Types.Jkind_mod_bounds.t and Axis_lattice.t *)

let level_of_areality (a : Mode.Regionality.Const.t) : int =
  match a with
  | Mode.Regionality.Const.Global -> 0
  | Mode.Regionality.Const.Regional -> 1
  | Mode.Regionality.Const.Local -> 2

let areality_of_level = function
  | 0 -> Mode.Regionality.Const.Global
  | 1 -> Mode.Regionality.Const.Regional
  | 2 -> Mode.Regionality.Const.Local
  | _ -> invalid_arg "Axis_lattice.areality_of_level"

let level_of_linearity (x : Mode.Linearity.Const.t) : int =
  match x with Mode.Linearity.Const.Many -> 0 | Mode.Linearity.Const.Once -> 1

let linearity_of_level = function
  | 0 -> Mode.Linearity.Const.Many
  | 1 -> Mode.Linearity.Const.Once
  | _ -> invalid_arg "Axis_lattice.linearity_of_level"

let level_of_uniqueness_monadic (x : Mode.Uniqueness.Const.t) : int =
  match x with
  | Mode.Uniqueness.Const.Unique -> 1
  | Mode.Uniqueness.Const.Aliased -> 0

let uniqueness_of_level_monadic = function
  | 0 -> Mode.Uniqueness.Const.Aliased
  | 1 -> Mode.Uniqueness.Const.Unique
  | _ -> invalid_arg "Axis_lattice.uniqueness_of_level_monadic"

let level_of_portability (x : Mode.Portability.Const.t) : int =
  match x with
  | Mode.Portability.Const.Portable -> 0
  | Mode.Portability.Const.Nonportable -> 1

let portability_of_level = function
  | 0 -> Mode.Portability.Const.Portable
  | 1 -> Mode.Portability.Const.Nonportable
  | _ -> invalid_arg "Axis_lattice.portability_of_level"

let level_of_contention_monadic (x : Mode.Contention.Const.t) : int =
  match x with
  | Mode.Contention.Const.Contended -> 0
  | Mode.Contention.Const.Shared -> 1
  | Mode.Contention.Const.Uncontended -> 2

let contention_of_level_monadic = function
  | 0 -> Mode.Contention.Const.Contended
  | 1 -> Mode.Contention.Const.Shared
  | 2 -> Mode.Contention.Const.Uncontended
  | _ -> invalid_arg "Axis_lattice.contention_of_level_monadic"

let level_of_yielding (x : Mode.Yielding.Const.t) : int =
  match x with
  | Mode.Yielding.Const.Unyielding -> 0
  | Mode.Yielding.Const.Yielding -> 1

let yielding_of_level = function
  | 0 -> Mode.Yielding.Const.Unyielding
  | 1 -> Mode.Yielding.Const.Yielding
  | _ -> invalid_arg "Axis_lattice.yielding_of_level"

let level_of_statefulness (x : Mode.Statefulness.Const.t) : int =
  match x with
  | Mode.Statefulness.Const.Stateless -> 0
  | Mode.Statefulness.Const.Observing -> 1
  | Mode.Statefulness.Const.Stateful -> 2

let statefulness_of_level = function
  | 0 -> Mode.Statefulness.Const.Stateless
  | 1 -> Mode.Statefulness.Const.Observing
  | 2 -> Mode.Statefulness.Const.Stateful
  | _ -> invalid_arg "Axis_lattice.statefulness_of_level"

let level_of_visibility_monadic (x : Mode.Visibility.Const.t) : int =
  match x with
  | Mode.Visibility.Const.Immutable -> 0
  | Mode.Visibility.Const.Read -> 1
  | Mode.Visibility.Const.Read_write -> 2

let visibility_of_level_monadic = function
  | 0 -> Mode.Visibility.Const.Immutable
  | 1 -> Mode.Visibility.Const.Read
  | 2 -> Mode.Visibility.Const.Read_write
  | _ -> invalid_arg "Axis_lattice.visibility_of_level_monadic"

let level_of_externality (x : Jkind_axis.Externality.t) : int =
  match x with
  | External -> 0
  | External64 -> 1
  | Internal -> 2

let externality_of_level = function
  | 0 -> Jkind_axis.Externality.External
  | 1 -> Jkind_axis.Externality.External64
  | 2 -> Jkind_axis.Externality.Internal
  | _ -> invalid_arg "Axis_lattice.externality_of_level"

let level_of_nullability (x : Jkind_axis.Nullability.t) : int =
  match x with Non_null -> 0 | Maybe_null -> 1

let nullability_of_level = function
  | 0 -> Jkind_axis.Nullability.Non_null
  | 1 -> Jkind_axis.Nullability.Maybe_null
  | _ -> invalid_arg "Axis_lattice.nullability_of_level"

let level_of_separability (x : Jkind_axis.Separability.t) : int =
  match x with
  | Non_float -> 0
  | Separable -> 1
  | Maybe_separable -> 2

let separability_of_level = function
  | 0 -> Jkind_axis.Separability.Non_float
  | 1 -> Jkind_axis.Separability.Separable
  | 2 -> Jkind_axis.Separability.Maybe_separable
  | _ -> invalid_arg "Axis_lattice.separability_of_level"

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
  Bit.encode ~levels

let to_mod_bounds (x : t) : Types.Jkind_mod_bounds.t =
  let lv = Bit.decode x in
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
