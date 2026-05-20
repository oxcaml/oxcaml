(* TEST
   compile_only = "true";
   ocamlopt_flags += " -flambda2-join-points -flambda2-join-algorithm=n-way";
   flambda2;
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-raw, dump-simplify;
   check-fexpr-dump;
 *)

(* This test is making sure that joins between [Null] and [This _] do not
   crash. We use an [@local] annotation to create a dumy continuation and
   ensure we are computing a join, which is otherwise not computed on function
   return. *)

let main x =
  let[@local] ret y = y in
  match x with
  | This _ -> ret 0
  | Null -> ret 1
