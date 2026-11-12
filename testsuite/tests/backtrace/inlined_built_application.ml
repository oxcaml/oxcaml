(* TEST
 flags = "-g";
 ocamlrunparam += ",b=1";
 ocamlopt_flags = "-O3";
 flambda2;
 native;
*)

(* Simplify rewrites these applications, which only appear once their callee is
   inlined, into terms built from the application it has already simplified
   (for [partial], a wrapper, which is then inlined at [p 2 3]). The frame of the
   inlined callee appears once in their backtraces, which are the same as with
   [-Oclassic].

   A partial application wrapper, whether built by Simplify or before (for
   [direct] and [labelled]), calls the function on behalf of the application
   of the wrapper: where the wrapper is inlined, the backtrace shows that
   application, not where the wrapper was built. *)

let[@inline never] check name n = if n > 0 then raise (Failure name) else n

let[@inline never] partial a b c = 1 + check "partial" (a + b + c)

let[@inline never] tuple (a, b) = 1 + check "tuple" (a + b)

let[@inline never] labelled ~a ~b c = 1 + check "labelled" (a + b + c)

let[@inline never] over a =
  let a = 1 + check "over" a in
  fun b -> a + b

let[@inline always] apply1 k x =
  let p = k x in
  1 + p 2 3

let[@inline always] apply_pair k x = 1 + k (x, x)

let[@inline always] apply2 k x = 1 + k x x

let[@inline never] via_partial x = 1 + apply1 partial x

let[@inline never] via_tuple x = 1 + apply_pair tuple x

let[@inline never] via_over x = 1 + apply2 over x

let[@inline never] direct x =
  let p = partial x in
  1 + p 2 3

let[@inline never] eta x =
  let p = labelled ~b:x in
  1 + p ~a:2 3

let[@inline never] run f =
  match f 1 with
  | (_ : int) -> ()
  | exception Failure name ->
    print_endline name;
    Printexc.print_backtrace stdout

let () =
  run via_partial;
  run via_tuple;
  run via_over;
  run direct;
  run eta
