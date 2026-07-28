(* TEST
 flags = "-extension layouts_beta";
 flambda2;
 compile_only = "true";
 ocamlopt_opt_exit_status = "2";
 setup-ocamlopt.opt-build-env;
 ocamlopt.opt;
*)

(* CR-soon lmaurer from Claude: this is a KNOWN BUG, recorded here as a failing
   test (the compiler currently crashes with [Invalid_argument "Array.sub"] in
   flambda2's [Lambda_to_flambda_primitives], so this test expects exit code 2).
   It is the inline-record sibling of the bug fixed by keeping live sorts in
   [Typedtree.record_sorts] (see unboxed_record_any_field_inference.ml): when a
   constructor's inline-record field has kind [any] and its instantiation is
   only discovered from the function body by inference, the field's sort and the
   mixed-block shape are computed during pattern typing, before the [any] field
   is resolved. The field read is then compiled as a plain value load
   ([field_imm]) while the projections downstream use the resolved
   unboxed-product sort. The concrete-annotation control [g] below compiles
   fine, which shows the staleness is introduced at use-site typing time, not at
   declaration time. When this is fixed, convert this file into a running test
   like unboxed_record_any_field_inference.ml. *)

type ('a : any) w = W of { id : int; value : 'a }

type t =
  #{ foo : int
   ; bar : string
   }

(* Control: concrete annotation, compiles and behaves correctly. *)
let g (W { id; value = t } : t w) =
  ignore (id : int);
  String.length t.#bar

(* Known bug: wildcard annotation, ICEs in the middle end. *)
let f (W { id; value = t } : _ w) =
  ignore (id : int);
  String.length t.#bar
