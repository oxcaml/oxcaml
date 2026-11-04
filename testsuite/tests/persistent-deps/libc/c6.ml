let z = B.h 1

(* Inlining B.h exposes its call to A.f, whose compilation unit description
   is then needed. *)
