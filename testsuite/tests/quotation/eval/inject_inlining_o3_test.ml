(* TEST
 include eval;
 set OCAML_EVAL_SHOW_WARNINGS = "1";
 reference = "${test_source_directory}/inject_inlining_o3_test.reference";
 flags = "-extension runtime_metaprogramming -O3 -w -53";
 native;
*)

(* As inject_inlining_test.ml, but with the first compilation in simplify
   mode (-O3): the approximations are reified during Simplify from the
   arguments' types (mostly as symbol aliases, since simplify mode lifts
   closures to symbols where possible).

   Checks that the approximations of injected values propagate to the
   runtime compilation of evaluated quotes: [@inlined] on a call to an
   injected function succeeds silently when the approximation (and hence
   the code) is known, and triggers warning 55 when it is not.  The control
   case uses [Sys.opaque_identity] to guarantee an unknowable function. *)

#syntax quotations on

(* Warning 53 is disabled (in the TEST block above) because the first
   (quote-building) compilation considers the [@inlined] attributes below
   misplaced; they are nonetheless preserved in the quotes, where they
   apply to the applications at evaluation time. *)

let top_level_double x = x * 2

(* The local-closure case is under a non-inlinable function, so that the
   closure is genuinely allocated per call and cannot be lifted to a symbol
   by Simplify (which it would be if it appeared in the toplevel module
   initializer). *)
let[@inline never] inject_local_closure () =
  let base = Sys.opaque_identity 40 in
  let add x = base + x in
  Eval.eval_with_injector (fun injector ->
      <[ ($(Eval.inject injector add) [@inlined]) 2 ]>)

let () =
  (* Control: no approximation can be known; warning 55 expected. *)
  let opaque = Sys.opaque_identity (fun x -> x + 1) in
  let c : int =
    Eval.eval_with_injector (fun injector ->
        <[ ($(Eval.inject injector opaque) [@inlined]) 1 ]>)
  in
  (* Top-level function: approximation known; no warning expected. *)
  let a : int =
    Eval.eval_with_injector (fun injector ->
        <[ ($(Eval.inject injector top_level_double) [@inlined]) 21 ]>)
  in
  (* Dynamic local closure without a symbol (it captures [base]): under -O3
     it cannot be lifted to a symbol and its code uses [my_closure], so
     reification currently degrades to Unknown (see inject_plan.md); warning
     55 expected. *)
  let b : int = inject_local_closure () in
  (* The warnings are emitted via [Format.err_formatter]; flush it before
     printing the results so that the interleaving is deterministic. *)
  Format.pp_print_flush Format.err_formatter ();
  Printf.printf "results: %d %d %d\n%!" c a b
