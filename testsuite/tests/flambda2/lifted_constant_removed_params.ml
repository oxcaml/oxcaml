(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags += " -O3";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
*)

(* Regression test for a bug in the placement of lifted constants relative to
   the [let]s introduced by the flow analysis to rebind alias-removed
   continuation parameters (see [Simplify_let_cont_expr.add_lets_around_handler]).

   The alias analysis removes continuation parameters whose canonical
   dominator is another simple, rebinding them inside the handler with
   [let param = dominator]. At unit toplevel, lifted constants placed around
   the same handler can reference such removed parameters. They were
   previously placed outside the rebinding [let]s, leaving the reference to
   the removed parameter unbound in the rebuilt term, which tripped the
   fatal error "The alias analysis marked the param ... as removed, but the
   free_names indicate it is actually used". Whether the crash manifested
   was sensitive to name-stamp ordering (e.g. the source file name), since
   that determines which member of an alias class becomes the canonical
   dominator.

   This file was minimized from real code (patdiff). *)

[@@@ocaml.warning "-6-20-26-27-32-37-39-60"]

external __dummy2__ : unit -> 'a = "%opaque"
external opaque : 'a -> 'a = "%opaque"

module Comparison_result = struct
  type t =
    | Binary_different of
        { prev_is_binary : bool
        ; next_is_binary : bool
        }

  let[@inline always] create config =
    let prev_is_binary, next_is_binary =
      match config with
      | true -> __dummy2__ ()
      | false -> let x =  __dummy2__ () in (x, (__dummy2__ () && __dummy2__ ()))
    in
    if next_is_binary
    then Binary_different { next_is_binary; prev_is_binary }
    else __dummy2__ ()
end

let (_ : Comparison_result.t) =
  let config = false in
  opaque (Comparison_result.create config)
