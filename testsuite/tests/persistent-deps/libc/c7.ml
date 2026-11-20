(* Without -no-alias-deps, the alias makes A a required global, although
   nothing refers to it at runtime. *)
module M = A

let z = B.h 1
