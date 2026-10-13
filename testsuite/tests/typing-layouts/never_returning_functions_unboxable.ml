(* TEST
 flambda2;
 unset OCAMLPARAM;
 {
   native;
 }{
   flags = "-O3";
   native;
 }
*)

(* Unboxing a parameter of a function that never returns raises no warning
   about its result. *)

external raise_any : ('a : any). exn -> 'a = "%raise"

exception Fail of float

let[@inline never] fail : float -> float =
  fun (x [@unboxable]) -> raise_any (Fail (x +. 1.))

let () =
  match fail (Sys.opaque_identity 1.5) with
  | x -> Printf.printf "returned %.1f\n" x
  | exception Fail x -> Printf.printf "Fail %.1f\n" x
