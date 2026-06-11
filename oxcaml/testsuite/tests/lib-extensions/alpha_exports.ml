(* TEST
 flags = "-extension-universe alpha";
 include stdlib_alpha;
 {
   bytecode;
 }{
   native;
 }
*)

open Stdlib_alpha

(* Check that the alpha extension library modules exist. *)
module Effect_reflection' = Effect_reflection
