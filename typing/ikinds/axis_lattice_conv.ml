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

let crossing_of_constants ~areality ~linearity ~uniqueness ~portability
    ~contention ~forkable ~yielding ~statefulness ~visibility ~staticity :
    Mode.Crossing.t =
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
      ~staticity:
        (Monadic.Atom.Modality
           (Mode.Modality.Monadic.Atom.Join_with staticity))
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

let of_mod_bounds (mb : Types.Jkind_mod_bounds.t) : Axis_lattice.t =
  let open Types.Jkind_mod_bounds in
  let boxed : Axis_lattice.boxed =
    { areality = areality_const mb;
      linearity = linearity_const mb;
      uniqueness = uniqueness_const mb;
      portability = portability_const mb;
      contention = contention_const mb;
      forkable = forkable_const mb;
      yielding = yielding_const mb;
      statefulness = statefulness_const mb;
      visibility = visibility_const mb;
      staticity = staticity_const mb;
      externality = externality mb;
      nullability = nullability mb;
      separability = separability mb
    }
  in
  Axis_lattice.of_boxed boxed

let to_mod_bounds (x : Axis_lattice.t) : Types.Jkind_mod_bounds.t =
  let ({ areality;
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
        Axis_lattice.boxed) =
    Axis_lattice.to_boxed x
  in
  let crossing =
    crossing_of_constants ~areality ~linearity ~uniqueness ~portability
      ~contention ~forkable ~yielding ~statefulness ~visibility ~staticity
  in
  Types.Jkind_mod_bounds.create crossing ~externality ~nullability ~separability
