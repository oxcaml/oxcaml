(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Translation from typed abstract syntax to lambda terms,
   for the module language *)

open Typedtree
open Lambda

module SL := Slambda

(* The triple here is the structure, the coercion from the raw structure to
   the main signature, and the coercion from the main signature to the argument
   signature (corresponding to the [structure], [coercion], and
   [argument_interface.ai_coercion_from_primary] fields from
   [Typedtree.implementation].)*)
(* CR lmaurer: This should just be taking [Typedtree.implementation]. But it
   can't, because [Opttoploop] calls it and doesn't have a full implementation.
   But [Opttoploop] _shouldn't_ be calling it, it should be calling
   [transl_store_phrases], because it's only storing phrases. But [Opttoploop]
   _should not exist anymore_, since upstream refactored the toplevel code.
   mshinwell: PR4527 has now removed transl_store* *)
val transl_implementation:
      Compilation_unit.t -> structure * module_coercion * module_coercion option
        -> loc:Location.t -> SL.program

(* Can only be used when targeting bytecode *)
val transl_toplevel_definition: structure -> lambda

val transl_package:
      Compilation_unit.t option list -> module_coercion -> int * lambda

type runtime_arg =
  | (* A module from which we need to project out the argument block *)
    Argument_block of {
      (* The compilation unit being passed as an argument *)
      ra_unit : Compilation_unit.t;
      (* The offset of its argument block, as advertised in its .cmo/.cmx *)
      ra_field_idx : int;
      (* The representation of the main block, which is needed to
         index into it *)
      ra_main_repr : module_representation;
    }
  | (* A module to pass in its entirety *)
    Main_module_block of Compilation_unit.t
  | Unit

val transl_instance:
      Compilation_unit.t -> runtime_args:runtime_arg list
        -> main_module_block_repr:module_representation
        -> arg_block_idx:int option
        -> SL.program

val toplevel_name: Ident.t -> string

val primitive_declarations: Primitive.description list ref

type error =
| Conflicting_inline_attributes
| Instantiating_packed of Compilation_unit.t

exception Error of Location.t * error

val report_error: Location.t -> error -> Location.error

val reset: unit -> unit
