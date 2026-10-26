(* TEST
 flambda2;
 flags = "-O3 -flambda2-result-types-all-functions -flambda2-join-algorithm=n-way";
 native;
*)

(* The result types of [f] say that if [f] returns then [b = false]. This must
   not be assumed in the exception handler. *)

exception Fail of string

let[@inline never] f b = if b then raise (Fail "k") else "x"

let () =
  match f true with
  | s -> print_endline s
  | exception Fail s -> print_endline ("Fail " ^ s)
