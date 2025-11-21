(* Axis lattice specialized from the generic product lattice (array-based). *)
module T = Product_lattice.Make (struct
  (* Align with all jkinds axes in OxCaml, in the same order
     as typing/jkind_axis.ml Axis_set.axis_index, and list values
     in the same increasing order as our encoded levels:
     0 Areality (Regionality): Global, Regional, Local -> 3
     1 Linearity: Many, Once -> 2
     2 Uniqueness (monadic): Aliased, Unique -> 2
     3 Portability: Portable, Nonportable -> 2
     4 Contention (monadic): Contended, Shared, Uncontended -> 3
     5 Forkable: Forkable, Unforkable -> 2
     6 Yielding: Unyielding, Yielding -> 2
     7 Statefulness: Stateless, Observing, Stateful -> 3
     8 Visibility (monadic): Immutable, Read, Read_write -> 3
     9 Externality: External, External64, Internal -> 3
     10 Nullability: Non_null, Maybe_null -> 2
     11 Separability: Non_float, Separable, Maybe_separable -> 3
  *)
  let axis_sizes = [| 3; 2; 2; 2; 3; 2; 2; 3; 3; 3; 2; 3 |]
end)

include T

let to_string = T.to_string

let of_levels ~(levels : int array) : t = encode ~levels

let to_levels (v : t) : int array = decode v

(* Axis-set encoding
   Map a set of relevant axes to a lattice element: relevant axes -> top level;
   non-relevant -> level 0. Ordering must match Axis_set.axis_index and our
   axis_sizes layout. *)
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
      | "forkable" -> Some 5
      | "yielding" -> Some 6
      | "statefulness" -> Some 7
      | "visibility" -> Some 8
      | "externality" -> Some 9
      | "nullability" -> Some 10
      | "separability" -> Some 11
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
        (* Convert the crossing axis into a modality axis to query
           per-axis constness. *)
        let (Mode.Modality.Axis.P axis_for_modality) =
          Mode.Crossing.Axis.(P axis |> to_modality)
        in
        let modality_on_axis =
          Mode.Modality.Const.proj axis_for_modality modality
        in
        not
          (Mode.Modality.Per_axis.is_constant axis_for_modality modality_on_axis)
      | Jkind_axis.Axis.Nonmodal Jkind_axis.Axis.Nonmodal.Externality -> true
      | Jkind_axis.Axis.Nonmodal Jkind_axis.Axis.Nonmodal.Nullability -> (
        match relevant_for_shallow with
        | `Relevant -> true
        | `Irrelevant -> false)
      | Jkind_axis.Axis.Nonmodal Jkind_axis.Axis.Nonmodal.Separability -> (
        match relevant_for_shallow with
        | `Relevant -> true
        | `Irrelevant -> false))

(* Directly produce an axis-lattice mask from a constant modality. *)
let mask_of_modality ~(relevant_for_shallow : [`Relevant | `Irrelevant])
    (modality : Mode.Modality.Const.t) : t =
  relevant_axes_of_modality ~relevant_for_shallow modality |> of_axis_set

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
    | Mode.Portability.Const.Nonportable -> 1

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

  let level_of_externality (x : Jkind_axis.Externality.t) : int =
    match x with
    | Jkind_axis.Externality.External -> 0
    | Jkind_axis.Externality.External64 -> 1
    | Jkind_axis.Externality.Internal -> 2

  let level_of_nullability (x : Jkind_axis.Nullability.t) : int =
    match x with
    | Jkind_axis.Nullability.Non_null -> 0
    | Jkind_axis.Nullability.Maybe_null -> 1

  let level_of_separability (x : Jkind_axis.Separability.t) : int =
    match x with
    | Jkind_axis.Separability.Non_float -> 0
    | Jkind_axis.Separability.Separable -> 1
    | Jkind_axis.Separability.Maybe_separable -> 2

  let areality_of_level = function
    | 0 -> Mode.Regionality.Const.Global
    | 1 -> Mode.Regionality.Const.Regional
    | 2 -> Mode.Regionality.Const.Local
    | _ -> invalid_arg "Axis_lattice_array.areality_of_level"

  let linearity_of_level = function
    | 0 -> Mode.Linearity.Const.Many
    | 1 -> Mode.Linearity.Const.Once
    | _ -> invalid_arg "Axis_lattice_array.linearity_of_level"

  let uniqueness_of_level_monadic = function
    | 0 -> Mode.Uniqueness.Const.Aliased
    | 1 -> Mode.Uniqueness.Const.Unique
    | _ -> invalid_arg "Axis_lattice_array.uniqueness_of_level_monadic"

  let portability_of_level = function
    | 0 -> Mode.Portability.Const.Portable
    | 1 -> Mode.Portability.Const.Nonportable
    | _ -> invalid_arg "Axis_lattice_array.portability_of_level"

  let contention_of_level_monadic = function
    | 0 -> Mode.Contention.Const.Contended
    | 1 -> Mode.Contention.Const.Shared
    | 2 -> Mode.Contention.Const.Uncontended
    | _ -> invalid_arg "Axis_lattice_array.contention_of_level_monadic"

  let forkable_of_level = function
    | 0 -> Mode.Forkable.Const.Forkable
    | 1 -> Mode.Forkable.Const.Unforkable
    | _ -> invalid_arg "Axis_lattice_array.forkable_of_level"

  let yielding_of_level = function
    | 0 -> Mode.Yielding.Const.Unyielding
    | 1 -> Mode.Yielding.Const.Yielding
    | _ -> invalid_arg "Axis_lattice_array.yielding_of_level"

  let statefulness_of_level = function
    | 0 -> Mode.Statefulness.Const.Stateless
    | 1 -> Mode.Statefulness.Const.Observing
    | 2 -> Mode.Statefulness.Const.Stateful
    | _ -> invalid_arg "Axis_lattice_array.statefulness_of_level"

  let visibility_of_level_monadic = function
    | 0 -> Mode.Visibility.Const.Immutable
    | 1 -> Mode.Visibility.Const.Read
    | 2 -> Mode.Visibility.Const.Read_write
    | _ -> invalid_arg "Axis_lattice_array.visibility_of_level_monadic"

  let externality_of_level = function
    | 0 -> Jkind_axis.Externality.External
    | 1 -> Jkind_axis.Externality.External64
    | 2 -> Jkind_axis.Externality.Internal
    | _ -> invalid_arg "Axis_lattice_array.externality_of_level"

  let nullability_of_level = function
    | 0 -> Jkind_axis.Nullability.Non_null
    | 1 -> Jkind_axis.Nullability.Maybe_null
    | _ -> invalid_arg "Axis_lattice_array.nullability_of_level"

  let separability_of_level = function
    | 0 -> Jkind_axis.Separability.Non_float
    | 1 -> Jkind_axis.Separability.Separable
    | 2 -> Jkind_axis.Separability.Maybe_separable
    | _ -> invalid_arg "Axis_lattice_array.separability_of_level"
end

let const_of_levels
    ~areality ~linearity ~uniqueness ~portability
    ~contention ~forkable ~yielding ~statefulness ~visibility
    ~externality ~nullability ~separability =
  let open Levels in
  encode
    ~levels:
      [| level_of_areality areality;
         level_of_linearity linearity;
         level_of_uniqueness_monadic uniqueness;
         level_of_portability portability;
         level_of_contention_monadic contention;
         level_of_forkable forkable;
         level_of_yielding yielding;
         level_of_statefulness statefulness;
         level_of_visibility_monadic visibility;
         level_of_externality externality;
         level_of_nullability nullability;
         level_of_separability separability
      |]

(* Lattice constant for non-float value base *)
let nonfloat_value : t =
  const_of_levels
    ~areality:Mode.Regionality.Const.max
    ~linearity:Mode.Linearity.Const.max
    ~uniqueness:Mode.Uniqueness.Const.Unique
    ~portability:Mode.Portability.Const.max
    ~contention:Mode.Contention.Const.Uncontended
    ~forkable:Mode.Forkable.Const.max
    ~yielding:Mode.Yielding.Const.max
    ~statefulness:Mode.Statefulness.Const.max
    ~visibility:Mode.Visibility.Const.Read_write
    ~externality:Jkind_axis.Externality.max
    ~nullability:Jkind_axis.Nullability.Non_null
    ~separability:Jkind_axis.Separability.Non_float

(* Convenience constants matching JK builtins for record bases. *)

let immutable_data : t =
  const_of_levels
    ~areality:Mode.Regionality.Const.max
    ~linearity:Mode.Linearity.Const.min
    ~uniqueness:Mode.Uniqueness.Const.Unique
    ~portability:Mode.Portability.Const.min
    ~contention:Mode.Contention.Const.Contended
    ~forkable:Mode.Forkable.Const.max
    ~yielding:Mode.Yielding.Const.min
    ~statefulness:Mode.Statefulness.Const.min
    ~visibility:Mode.Visibility.Const.Immutable
    ~externality:Jkind_axis.Externality.max
    ~nullability:Jkind_axis.Nullability.Non_null
    ~separability:Jkind_axis.Separability.Non_float

let mutable_data : t =
  const_of_levels
    ~areality:Mode.Regionality.Const.max
    ~linearity:Mode.Linearity.Const.min
    ~uniqueness:Mode.Uniqueness.Const.Unique
    ~portability:Mode.Portability.Const.min
    ~contention:Mode.Contention.Const.Uncontended
    ~forkable:Mode.Forkable.Const.max
    ~yielding:Mode.Yielding.Const.min
    ~statefulness:Mode.Statefulness.Const.min
    ~visibility:Mode.Visibility.Const.Read_write
    ~externality:Jkind_axis.Externality.max
    ~nullability:Jkind_axis.Nullability.Non_null
    ~separability:Jkind_axis.Separability.Non_float

(* Matches JK Builtin.value: boxed value, Non_null and Separable;
   no mode-crossing. *)
let value : t =
  const_of_levels
    ~areality:Mode.Regionality.Const.max
    ~linearity:Mode.Linearity.Const.max
    ~uniqueness:Mode.Uniqueness.Const.Unique
    ~portability:Mode.Portability.Const.max
    ~contention:Mode.Contention.Const.Uncontended
    ~forkable:Mode.Forkable.Const.max
    ~yielding:Mode.Yielding.Const.max
    ~statefulness:Mode.Statefulness.Const.max
    ~visibility:Mode.Visibility.Const.Read_write
    ~externality:Jkind_axis.Externality.max
    ~nullability:Jkind_axis.Nullability.Non_null
    ~separability:Jkind_axis.Separability.Separable

let arrow : t =
  const_of_levels
    ~areality:Mode.Regionality.Const.max
    ~linearity:Mode.Linearity.Const.max
    ~uniqueness:Mode.Uniqueness.Const.Aliased
    ~portability:Mode.Portability.Const.max
    ~contention:Mode.Contention.Const.Contended
    ~forkable:Mode.Forkable.Const.max
    ~yielding:Mode.Yielding.Const.max
    ~statefulness:Mode.Statefulness.Const.max
    ~visibility:Mode.Visibility.Const.Immutable
    ~externality:Jkind_axis.Externality.max
    ~nullability:Jkind_axis.Nullability.Non_null
    ~separability:Jkind_axis.Separability.Non_float

(* A lattice constant matching JK Builtin.immediate (mode-crosses everything,
   with nullability Non_null and separability Non_float). *)
let immediate : t =
  const_of_levels
    ~areality:Mode.Regionality.Const.min
    ~linearity:Mode.Linearity.Const.min
    ~uniqueness:Mode.Uniqueness.Const.Aliased
    ~portability:Mode.Portability.Const.min
    ~contention:Mode.Contention.Const.Contended
    ~forkable:Mode.Forkable.Const.max
    ~yielding:Mode.Yielding.Const.min
    ~statefulness:Mode.Statefulness.Const.min
    ~visibility:Mode.Visibility.Const.Immutable
    ~externality:Jkind_axis.Externality.min
    ~nullability:Jkind_axis.Nullability.Non_null
    ~separability:Jkind_axis.Separability.Non_float

(* Matches JK for_object: legacy on comonadic axes, max on monadic axes;
   Non_null, Non_float. *)
let object_legacy : t =
  let ({ linearity; areality; portability; forkable; yielding; statefulness }
        : Mode.Value.Comonadic.Const.t) =
    Mode.Value.Comonadic.Const.legacy
  in
  const_of_levels
    ~linearity
    ~areality
    ~uniqueness:Mode.Uniqueness.Const.Unique
    ~portability
    ~contention:Mode.Contention.Const.Uncontended
    ~forkable
    ~yielding
    ~statefulness
    ~visibility:Mode.Visibility.Const.Read_write
    ~externality:Jkind_axis.Externality.max
    ~nullability:Jkind_axis.Nullability.Non_null
    ~separability:Jkind_axis.Separability.Non_float

let axis_number_to_axis_packed (axis_number : int) : Jkind_axis.Axis.packed =
  (* Match the ordering used by Jkind_axis.Axis_set.axis_index *)
  let open Mode.Crossing.Axis in
  match axis_number with
  | 0 -> Jkind_axis.Axis.Pack (Jkind_axis.Axis.Modal (Comonadic Areality))
  | 1 -> Jkind_axis.Axis.Pack (Jkind_axis.Axis.Modal (Comonadic Linearity))
  | 2 -> Jkind_axis.Axis.Pack (Jkind_axis.Axis.Modal (Monadic Uniqueness))
  | 3 -> Jkind_axis.Axis.Pack (Jkind_axis.Axis.Modal (Comonadic Portability))
  | 4 -> Jkind_axis.Axis.Pack (Jkind_axis.Axis.Modal (Monadic Contention))
  | 5 -> Jkind_axis.Axis.Pack (Jkind_axis.Axis.Modal (Comonadic Forkable))
  | 6 -> Jkind_axis.Axis.Pack (Jkind_axis.Axis.Modal (Comonadic Yielding))
  | 7 -> Jkind_axis.Axis.Pack (Jkind_axis.Axis.Modal (Comonadic Statefulness))
  | 8 -> Jkind_axis.Axis.Pack (Jkind_axis.Axis.Modal (Monadic Visibility))
  | 9 ->
    Jkind_axis.Axis.Pack
      (Jkind_axis.Axis.Nonmodal Jkind_axis.Axis.Nonmodal.Externality)
  | 10 ->
    Jkind_axis.Axis.Pack
      (Jkind_axis.Axis.Nonmodal Jkind_axis.Axis.Nonmodal.Nullability)
  | 11 ->
    Jkind_axis.Axis.Pack
      (Jkind_axis.Axis.Nonmodal Jkind_axis.Axis.Nonmodal.Separability)
  | _ -> failwith "axis_number_to_axis_packed: invalid axis number"
