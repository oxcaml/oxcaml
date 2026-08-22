(* Restrict a type declaration's kind to its own type parameters: remove the
   named-type variables from the declaration's ikind, then rebuild the
   settled formula as a jkind for rendering. *)

open Std

module Ldd = Types.Ldd

(* Suggestions only mention the declaration's own type parameters, so the
   ikind formula must be re-expressed without its named-type variables.
   Joins and meets are monotone, so substituting top for every variable
   moves the formula up the lattice. *)
let remove_named_types ({ base; coeffs } : Types.constructor_ikind) :
    Types.constructor_ikind =
  let settle node = Ldd.map_rigid (fun _ -> Ldd.const Axis_lattice.top) node in
  { base = settle base; coeffs = Array.map settle coeffs }

(* The axes on which [mask] lets a with-bound contribute. Axes where
   [mask] sits strictly between bot and top are rounded up to fully
   relevant: the bound then contributes more, which only weakens the
   suggested kind. *)
let relevant_axes_of_mask (mask : Axis_lattice.t) : Jkind_axis.Axis_set.t =
  Jkind_axis.Axis_set.create ~f:(fun ~axis:(Jkind_axis.Axis.Pack axis) ->
      let single =
        Axis_lattice.of_axis_set (Jkind_axis.Axis_set.singleton axis)
      in
      not (Axis_lattice.equal (Axis_lattice.meet mask single) Axis_lattice.bot))

(* The with-bound for one type parameter, or [None] when [coeff]
   contributes nothing beyond [base]. *)
let parameter_bound ~base param coeff =
  let coeff = Ldd.sub_subsets coeff base in
  let mask = Ldd.round_up coeff in
  if Axis_lattice.equal mask Axis_lattice.bot then None
  else
    Some
      ( param,
        { Types.With_bounds_type_info.relevant_axes = relevant_axes_of_mask mask
        } )

(* Rebuild a jkind from a settled ikind: one whose base and coefficients
   are constants, as produced by [remove_named_types]. Note, that we cannot
   convert arbitrary ikinds to jkinds, but this works for the narrow set
   of ikinds we have after conversion. *)
let jkind_of_ikind ~layout ~(decl : Types.type_declaration)
    ({ base; coeffs } : Types.constructor_ikind) : Types.jkind_l =
  let mod_bounds = Jkind.Mod_bounds.of_axis_lattice (Ldd.round_up base) in
  let bounds =
    List.map2 decl.type_params (Array.to_list coeffs) ~f:(parameter_bound ~base)
    |> List.filter_map ~f:(fun bound -> bound)
  in
  let with_bounds : (Allowance.allowed * Allowance.disallowed) Types.with_bounds
      =
    match bounds with
    | [] -> No_with_bounds
    | bounds -> With_bounds (Types.With_bounds_types.of_list bounds)
  in
  { jkind = { base = Layout layout; mod_bounds; with_bounds };
    annotation = None;
    quality = Not_best;
    has_warned = false;
    history = Creation (Concrete_creation Merlin);
    ran_out_of_fuel_during_normalize = false
  }

let restrict_to_parameters ~env ~(decl : Types.type_declaration)
    (ikind : Types.type_ikind) : Types.jkind_l option =
  match (ikind, Jkind.extract_layout env decl.type_jkind) with
  | No_constructor_ikind _, _ | _, Error _ -> None
  | Constructor_ikind ikind, Ok layout ->
    Some (jkind_of_ikind ~layout ~decl (remove_named_types ikind))
