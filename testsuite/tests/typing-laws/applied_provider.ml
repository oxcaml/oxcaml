module F (X : sig end) = struct let x = ref 0 exception E end
module A = struct end
module Id (X : sig val x : int ref exception E end) = X
module M = Id (F (A))
module N = Id (F (A))
module Bound = F (A)
module P = Id (Bound)
module Q = Id (Bound)
