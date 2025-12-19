let crossing_of_constants ~areality ~linearity ~uniqueness ~portability
    ~contention ~forkable ~yielding ~statefulness ~visibility ~staticity :
    Mode.Crossing.t =
  let open Mode.Crossing in
  let monadic =
    Monadic.create
      ~uniqueness:
        (Monadic.Atom.Modality (Mode.Modality.Monadic.Atom.Join_with uniqueness))
      ~contention:
        (Monadic.Atom.Modality (Mode.Modality.Monadic.Atom.Join_with contention))
      ~visibility:
        (Monadic.Atom.Modality (Mode.Modality.Monadic.Atom.Join_with visibility))
      ~staticity:
        (Monadic.Atom.Modality (Mode.Modality.Monadic.Atom.Join_with staticity))
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
      ~forkable:
        (Comonadic.Atom.Modality
           (Mode.Modality.Comonadic.Atom.Meet_with forkable))
      ~yielding:
        (Comonadic.Atom.Modality
           (Mode.Modality.Comonadic.Atom.Meet_with yielding))
      ~statefulness:
        (Comonadic.Atom.Modality
           (Mode.Modality.Comonadic.Atom.Meet_with statefulness))
  in
  { monadic; comonadic }

let of_mod_bounds (mb : Types.Jkind_mod_bounds.t) : Axis_lattice_array.t =
  let open Types.Jkind_mod_bounds in
  let open Axis_lattice_array.Levels in
  let levels =
    [| level_of_areality (areality_const mb);
       level_of_linearity (linearity_const mb);
       level_of_uniqueness_monadic (uniqueness_const mb);
       level_of_portability (portability_const mb);
       level_of_contention_monadic (contention_const mb);
       level_of_forkable (forkable_const mb);
       level_of_yielding (yielding_const mb);
       level_of_statefulness (statefulness_const mb);
       level_of_visibility_monadic (visibility_const mb);
       level_of_staticity_monadic (Types.Jkind_mod_bounds.staticity_const mb);
       level_of_externality (externality mb);
       level_of_nullability (nullability mb);
       level_of_separability (separability mb)
    |]
  in
  Axis_lattice_array.of_levels ~levels

let to_mod_bounds (v : Axis_lattice_array.t) : Types.Jkind_mod_bounds.t =
  let open Axis_lattice_array.Levels in
  let lv = Axis_lattice_array.to_levels v in
  let areality = areality_of_level lv.(0) in
  let linearity = linearity_of_level lv.(1) in
  let uniqueness = uniqueness_of_level_monadic lv.(2) in
  let portability = portability_of_level lv.(3) in
  let contention = contention_of_level_monadic lv.(4) in
  let forkable = forkable_of_level lv.(5) in
  let yielding = yielding_of_level lv.(6) in
  let statefulness = statefulness_of_level lv.(7) in
  let visibility = visibility_of_level_monadic lv.(8) in
  let staticity = staticity_of_level_monadic lv.(9) in
  let externality = externality_of_level lv.(10) in
  let nullability = nullability_of_level lv.(11) in
  let separability = separability_of_level lv.(12) in
  let crossing =
    crossing_of_constants ~areality ~linearity ~uniqueness ~portability
      ~contention ~forkable ~yielding ~statefulness ~visibility ~staticity
  in
  Types.Jkind_mod_bounds.create crossing ~externality ~nullability ~separability
