(* TEST
 flags = "-g";
 ocamlrunparam += ",b=1";
 ocamlopt_flags = "-O3";
 flambda2;
 native;
*)

(* Simplify rewrites these applications, which only appear once their callee is
   inlined, into terms built from the application it has already simplified.
   The frame of the inlined callee appears once in their backtraces. Partial
   applications are left out (see the CR in
   [Simplify_apply_expr.simplify_direct_partial_application]). *)

let[@inline never] check name n = if n > 0 then raise (Failure name) else n

let[@inline never] tuple (a, b) = 1 + check "tuple" (a + b)

let[@inline never] over a =
  let a = 1 + check "over" a in
  fun b -> a + b

let[@inline always] apply_pair k x = 1 + k (x, x)

let[@inline always] apply2 k x = 1 + k x x

let[@inline never] via_tuple x = 1 + apply_pair tuple x

let[@inline never] via_over x = 1 + apply2 over x

let[@inline never] run f =
  match f 1 with
  | (_ : int) -> ()
  | exception Failure name ->
    print_endline name;
    Printexc.print_backtrace stdout

let () =
  run via_tuple;
  run via_over
