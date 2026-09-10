(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(** The per-compilation-unit inputs to [compute]. They are recorded at traverse
    time (and, for LTO, serialised into the .cmr file) so that the solve-time
    computation does not need access to code metadata, which would require
    loading .cmx files. *)
module Inputs : sig
  (** The code metadata needed when laying out function slots. *)
  type code_info =
    { function_slot_size : int;
      dbg : Debuginfo.t
    }

  type t =
    { free_names : Name_occurrences.t;
          (** The free names of the whole compilation unit as output by
              simplify. *)
      closure_function_decls :
        Function_declarations.code_id_in_function_declaration
        Code_id_or_name.Map.t;
      code_info : code_info Code_id.Map.t
          (** Info for every code ID appearing in [closure_function_decls]. *)
    }

  val create :
    free_names:Name_occurrences.t ->
    closure_function_decls:
      Function_declarations.code_id_in_function_declaration
      Code_id_or_name.Map.t ->
    code_deps:Traverse_acc.code_dep Code_id.Map.t ->
    get_code_metadata:(Code_id.t -> Code_metadata.t) ->
    t

  val empty : t

  (** Combine the inputs of several compilation units for a whole-program (LTO)
      computation. *)
  val union : t -> t -> t

  val ids_for_export : t -> Ids_for_export.t

  val apply_renaming : t -> Renaming.t -> t
end

(** Compute the slot offsets of the sets of closures that will be built after
    rewriting. This runs at solve time: for LTO, [inputs] is the union of the
    participating units' inputs and [is_local_compilation_unit] is membership of
    the set of participants, so that one consistent assignment of offsets is
    computed for the whole program. *)
val compute :
  inputs:Inputs.t ->
  is_local_compilation_unit:(Compilation_unit.t -> bool) ->
  Unboxing_analysis.result ->
  Slot_offsets.result
