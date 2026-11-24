(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Introduce closures into CPS code, producing Flambda. *)

module IR = Closure_conversion_aux.IR
module Env = Closure_conversion_aux.Env
module Acc = Closure_conversion_aux.Acc
module Function_decl = Closure_conversion_aux.Function_decls.Function_decl
module Expr_with_acc = Closure_conversion_aux.Expr_with_acc

val close_let :
  Acc.t ->
  Env.t ->
  (Ident.t * Flambda_debug_uid.t * Flambda_kind.With_subkind.t) list ->
  IR.user_visible ->
  IR.named ->
  body:(Acc.t -> Env.t -> Expr_with_acc.t) ->
  Expr_with_acc.t

val close_let_rec :
  Acc.t ->
  Env.t ->
  function_declarations:Function_decl.t list ->
  body:(Acc.t -> Env.t -> Expr_with_acc.t) ->
  current_alloc_region:Ident.t ->
  current_region:Ident.t option ->
  Expr_with_acc.t

(** The unit's [Lambda.Lcode] bindings, with the templates the code
    instantiates, must all be declared before any of them is closed by
    [close_lcodes]. *)
val declare_lcodes :
  Env.t -> (Ident.t * Template_id.t * Function_decl.t) list -> Env.t

(** Close the code of instantiations of templates, which have no closure. In
    classic mode, the code of an instantiation of one of the unit's templates is
    only closed at the template's definition. *)
val close_lcodes :
  Acc.t -> Env.t -> (Ident.t * Template_id.t * Function_decl.t) list -> Acc.t

(** In classic mode, close the code of the instantiations of templates whose
    definitions were never reached. *)
val close_lcodes_of_unreached_templates : Acc.t -> Acc.t

val close_let_cont :
  Acc.t ->
  Env.t ->
  name:Continuation.t ->
  is_exn_handler:bool ->
  params:
    (Ident.t
    * Flambda_debug_uid.t
    * IR.user_visible
    * Flambda_kind.With_subkind.t)
    list ->
  recursive:Asttypes.rec_flag ->
  handler:(Acc.t -> Env.t -> Expr_with_acc.t) ->
  body:(Acc.t -> Env.t -> Expr_with_acc.t) ->
  Expr_with_acc.t

val close_apply : Acc.t -> Env.t -> IR.apply -> Expr_with_acc.t

val close_apply_cont :
  Acc.t ->
  Env.t ->
  dbg:Debuginfo.t ->
  Continuation.t ->
  IR.trap_action option ->
  IR.simple list ->
  Expr_with_acc.t

val close_switch :
  Acc.t ->
  Env.t ->
  condition_dbg:Debuginfo.t ->
  Ident.t ->
  IR.switch ->
  Expr_with_acc.t

val close_raise :
  Acc.t ->
  Env.t ->
  raise_kind:Lambda.raise_kind ->
  arg:IR.simple ->
  dbg:Debuginfo.t ->
  IR.exn_continuation ->
  Expr_with_acc.t

type 'a close_program_metadata =
  | Normal : [`Normal] close_program_metadata
  | Classic :
      (Exported_code.t
      * Code_or_metadata.t Value_approximation.t Symbol.Map.t
      * Name_occurrences.t
      * Slot_offsets.t
      * Symbol.Set.t)
      -> [`Classic] close_program_metadata
      (** The last component is the set of keys of the unit's templates (see
          [Flambda_primitive.Template_env]), whose approximations are those of
          the templates' environments. *)

type 'a close_program_result =
  { unit : Flambda_unit.t;
    metadata : 'a close_program_metadata;
    code_slot_offsets : Slot_offsets.t Code_id.Map.t
  }

val close_program :
  mode:'mode Flambda_features.mode ->
  machine_width:Target_system.Machine_width.t ->
  big_endian:bool ->
  cmx_loader:Flambda_cmx.loader ->
  compilation_unit:Compilation_unit.t ->
  module_repr:Lambda.module_representation ->
  program:(Acc.t -> Env.t -> Expr_with_acc.t) ->
  prog_return_cont:Continuation.t ->
  exn_continuation:Continuation.t ->
  toplevel_my_alloc_region:Ident.t ->
  'mode close_program_result
