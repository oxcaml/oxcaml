(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Hooks allow to inspect the IR produced by a pass without altering
   the compilation pipeline.

   Hooks are allowed to inspect the data but are prohibited from
   altering it. If one hook were to mutate the data there's no guarantee
   of how the compiler would behave.
   (The exception is the Parse_tree hooks, which can modify the AST like a ppx)
   Several hooks can be registered for the same pass. There's no guarantees
   on the order of execution of hooks.
   When one IR is the output of several passes, the hooks are usually called
   on the latest version of the IR (the exception being passes marked as "raw",
   where corresponding hooks are called on the earliest version of the IR).
*)

type (_,_) pass =
  | Parse_tree_intf : (Parsetree.signature, Parsetree.signature) pass
  | Parse_tree_impl : (Parsetree.structure, Parsetree.structure) pass
  | Typed_tree_intf : (Typedtree.signature,unit) pass
  | Typed_tree_impl : (Typedtree.implementation,unit) pass
  | Raw_lambda : (Lambda.program,unit) pass
  | Lambda : (Lambda.program,unit) pass
  | Raw_flambda2 : (Flambda2_terms.Flambda_unit.t,unit) pass
  | Flambda2 : (Flambda2_terms.Flambda_unit.t,unit) pass
  | Reaped_flambda2 : (Flambda2_terms.Flambda_unit.t,unit) pass

  | Linear : (Linear.fundecl,unit) pass
  | Cfg_combine : (Cfg_with_layout.t,unit) pass
  | Cfg_cse : (Cfg_with_layout.t,unit) pass
  | Cfg : (Cfg_with_layout.t,unit) pass
  | Cmm : (Cmm.phrase list,unit) pass

  | Inlining_tree :
      (Flambda2_simplify_shared.Inlining_report.Inlining_tree.t,unit) pass
  | Check_allocations : (Zero_alloc_checker.iter_witnesses,unit) pass

(* Register a new hook for [pass]. *)
val register : ('a,'out) pass -> ('a -> 'out) -> unit

(* Execute the hooks registered for [pass]. *)
val execute : ('a,'out) pass -> 'a -> 'out

val execute_and_pipe : ('a,unit) pass -> 'a -> 'a

(* Remove all hooks registered for [pass] *)
val clear : ('a,_) pass -> unit
