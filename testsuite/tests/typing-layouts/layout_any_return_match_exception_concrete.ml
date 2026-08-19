(* TEST
 flags = "-extension layouts_beta";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

(* A match with exception cases whose result is concrete is a direct return
   site of a return-any function: it unifies with the function's return sort
   and compiles through the ordinary exception-handler join. *)

type ('a : any) witness = Int : int witness

let[@inline never] f : type (a : any). a witness -> a = function
  | Int -> (
    match Sys.opaque_identity 1 with
    | 1 -> 42
    | _ -> 0
    | exception Not_found -> 1)

let[@inline never] plain_match_exception () =
  match Sys.opaque_identity 2 with
  | 2 -> 7
  | _ -> 0
  | exception Not_found -> 1

let () =
  assert (f Int = 42);
  assert (plain_match_exception () = 7)
