(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                            Gabriel Radanne                             *)
(*                                                                        *)
(*   Copyright 2018 Gabriel Radanne                                       *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Common compilation pipeline between bytecode and native. *)

(** {2 Initialization} *)

(** Information needed to compile a file. *)
type info =
  { target : Unit_info.t;
    module_name : Compilation_unit.t;
    env : Env.t;
    ppf_dump : Format.formatter;
    tool_name : string;
    native : bool
  }

type compilation_unit_or_inferred =
  | Exactly of Compilation_unit.t
  | Inferred_from_output_prefix

(** [with_info ~native ~tool_name ~source_file ~output_prefix ~dump_ext k]
   invokes its continuation [k] with an [info] structure built from
   its input, after initializing various global variables. This info
   structure and the initialized global state are not valid anymore
   after the continuation returns.

   Due to current implementation limitations in the compiler, it is
   unsafe to try to compile several distinct compilation units by
   calling [with_info] several times.
*)
val with_info :
  native:bool ->
  tool_name:string ->
  source_file:string ->
  output_prefix:string ->
  compilation_unit:compilation_unit_or_inferred ->
  kind:Unit_info.intf_or_impl ->
  dump_ext:string ->
  (info -> 'a) ->
  'a

(** {2 Interfaces} *)

(** [parse_intf info] parses an interface (usually an [.mli] file). *)
val parse_intf : info -> Parsetree.signature

(** [typecheck_intf info parsetree] typechecks an interface and returns
    the typedtree of the associated signature.
*)
val typecheck_intf :
  info -> Parsetree.signature -> Misc.alerts * Typedtree.signature

(** [emit_signature info parsetree typedtree] emits the [.cmi] file
    containing the given signature.
*)
val emit_signature : info -> Misc.alerts -> Typedtree.signature -> unit

(** The complete compilation pipeline for interfaces. *)
val interface :
  hook_parse_tree:(Parsetree.signature -> unit) ->
  hook_typed_tree:(Typedtree.signature -> unit) ->
  info ->
  unit

(** {2 Implementations} *)

(** [parse_impl info] parses an implementation (usually an [.ml] file). *)
val parse_impl : info -> Parsetree.structure

(** [typecheck_impl info parsetree] typechecks an implementation and returns
    the typedtree of the associated module, its public interface, and a
    coercion against that public interface.
*)
val typecheck_impl : info -> Parsetree.structure -> Typedtree.implementation

(** The complete compilation pipeline for implementations. *)
val implementation :
  hook_parse_tree:(Parsetree.structure -> unit) ->
  hook_typed_tree:(Typedtree.implementation -> unit) ->
  info ->
  backend:(info -> Typedtree.implementation -> unit) ->
  unit
