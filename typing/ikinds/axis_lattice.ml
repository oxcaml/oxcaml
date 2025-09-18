(* Axis lattice specialized from the generic product lattice. *)
module T = Product_lattice.Make (struct
  (* Align with all jkinds axes in OxCaml, in the same order
     as typing/jkind_axis.ml Axis_set.axis_index, and list values
     in the same increasing order as our encoded levels:
     0 Areality (Regionality): Global, Regional, Local -> 3
     1 Linearity: Many, Once -> 2
     2 Uniqueness (monadic): Aliased, Unique -> 2
     3 Portability: Portable, Nonportable -> 2
     4 Contention (monadic): Contended, Shared, Uncontended -> 3
     5 Yielding: Unyielding, Yielding -> 2
     6 Statefulness: Stateless, Observing, Stateful -> 3
     7 Visibility (monadic): Immutable, Read, Read_write -> 3
     8 Externality: External, External64, Internal -> 3
     9 Nullability: Non_null, Maybe_null -> 2
     10 Separability: Non_float, Separable, Maybe_separable -> 3
  *)
  let axis_sizes = [| 3; 2; 2; 2; 3; 2; 3; 3; 3; 2; 3 |]
end)

include T

let to_string = T.to_string

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

(* For monadic axes, the semantic ordering is flipped (we compare using the
   op-lattice). When encoding into our numeric lattice where higher means
   "more permissive/top", flip the levels for monadic axes. *)
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
  | Mode.Contention.Const.Uncontended -> 2
  | Mode.Contention.Const.Shared -> 1
  | Mode.Contention.Const.Contended -> 0

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
  | Mode.Visibility.Const.Read_write -> 2
  | Mode.Visibility.Const.Read -> 1
  | Mode.Visibility.Const.Immutable -> 0

let visibility_of_level_monadic = function
  | 0 -> Mode.Visibility.Const.Immutable
  | 1 -> Mode.Visibility.Const.Read
  | 2 -> Mode.Visibility.Const.Read_write
  | _ -> invalid_arg "Axis_lattice.visibility_of_level_monadic"

let level_of_externality (x : Jkind_axis.Externality.t) : int =
  match x with
  | Jkind_axis.Externality.External -> 0
  | Jkind_axis.Externality.External64 -> 1
  | Jkind_axis.Externality.Internal -> 2

let externality_of_level = function
  | 0 -> Jkind_axis.Externality.External
  | 1 -> Jkind_axis.Externality.External64
  | 2 -> Jkind_axis.Externality.Internal
  | _ -> invalid_arg "Axis_lattice.externality_of_level"

let level_of_nullability (x : Jkind_axis.Nullability.t) : int =
  match x with
  | Jkind_axis.Nullability.Non_null -> 0
  | Jkind_axis.Nullability.Maybe_null -> 1

let nullability_of_level = function
  | 0 -> Jkind_axis.Nullability.Non_null
  | 1 -> Jkind_axis.Nullability.Maybe_null
  | _ -> invalid_arg "Axis_lattice.nullability_of_level"

let level_of_separability (x : Jkind_axis.Separability.t) : int =
  match x with
  | Jkind_axis.Separability.Non_float -> 0
  | Jkind_axis.Separability.Separable -> 1
  | Jkind_axis.Separability.Maybe_separable -> 2

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
  let levels =
    [| level_of_areality (Types.Jkind_mod_bounds.areality_const mb);
       level_of_linearity (Types.Jkind_mod_bounds.linearity_const mb);
       level_of_uniqueness_monadic (Types.Jkind_mod_bounds.uniqueness_const mb);
       level_of_portability (Types.Jkind_mod_bounds.portability_const mb);
       level_of_contention_monadic (Types.Jkind_mod_bounds.contention_const mb);
       level_of_yielding (Types.Jkind_mod_bounds.yielding_const mb);
       level_of_statefulness (Types.Jkind_mod_bounds.statefulness_const mb);
       level_of_visibility_monadic (Types.Jkind_mod_bounds.visibility_const mb);
       level_of_externality (Types.Jkind_mod_bounds.externality mb);
       level_of_nullability (Types.Jkind_mod_bounds.nullability mb);
       level_of_separability (Types.Jkind_mod_bounds.separability mb)
    |]
  in
  encode ~levels

let to_mod_bounds (v : t) : Types.Jkind_mod_bounds.t =
  let lv = decode v in
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

(* Lattice constant for non-float value base *)
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
    Types.Jkind_mod_bounds.create crossing ~externality:Jkind_axis.Externality.max
      ~nullability:Jkind_axis.Nullability.Non_null
      ~separability:Jkind_axis.Separability.Non_float
  in
  of_mod_bounds mb

(* Convenience constants matching JK builtins for record bases. *)

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
    Types.Jkind_mod_bounds.create crossing ~externality:Jkind_axis.Externality.max
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
    Types.Jkind_mod_bounds.create crossing ~externality:Jkind_axis.Externality.max
      ~nullability:Jkind_axis.Nullability.Non_null
      ~separability:Jkind_axis.Separability.Non_float
  in
  of_mod_bounds mb

(* Matches JK Builtin.value: boxed value, Non_null and Separable;
   no mode-crossing. *)
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
    Types.Jkind_mod_bounds.create crossing ~externality:Jkind_axis.Externality.max
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
    Types.Jkind_mod_bounds.create crossing ~externality:Jkind_axis.Externality.max
      ~nullability:Jkind_axis.Nullability.Non_null
      ~separability:Jkind_axis.Separability.Non_float
  in
  of_mod_bounds mb

(* A lattice constant matching JK Builtin.immediate (mode-crosses everything,
   with nullability Non_null and separability Non_float). *)
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
    Types.Jkind_mod_bounds.create crossing ~externality:Jkind_axis.Externality.min
      ~nullability:Jkind_axis.Nullability.Non_null
      ~separability:Jkind_axis.Separability.Non_float
  in
  of_mod_bounds mb

(* Matches JK for_object: legacy on comonadic axes, max on monadic axes;
   Non_null, Non_float. *)
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
    Types.Jkind_mod_bounds.create crossing ~externality:Jkind_axis.Externality.max
      ~nullability:Jkind_axis.Nullability.Non_null
      ~separability:Jkind_axis.Separability.Non_float
  in
  of_mod_bounds mb

let axis_number_to_axis_packed (axis_number : int) : Jkind_axis.Axis.packed =
  (* Match the ordering used by Jkind_axis.Axis_set.axis_index *)
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
