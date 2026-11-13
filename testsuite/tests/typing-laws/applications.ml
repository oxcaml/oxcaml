(* TEST
 flags = "-extension laws";
 expect;
*)

(* The tests below use the functor [F] applied to [A] twice: once as [M] and
   once as [N]. Their types are shared ([F(A).t]), but each application
   results in a new module with its own values. To drive this point home,
   the functor is stateful and the values are truly different at runtime:
   [M.x] is [1], whereas [N.x] is [2]. *)

module F (X : sig val r : int ref end) = struct
  type t = A | B
  let x = incr X.r; !X.r
end
module A = struct let r = ref 0 end
module M = F (A)
module N = F (A)
let _ = (M.x, N.x)
[%%expect {|
module F :
  functor (X : sig val r : int ref end) -> sig type t = A | B val x : int end
module A : sig val r : int ref end
module M : sig type t = F(A).t = A | B val x : int end
module N : sig type t = F(A).t = A | B val x : int end
- : int * int = (1, 2)
|}]

(* Although [M] and [N] are both results of [F (A)], their values are not
   identified: [M.x = M.x] does not satisfy [M.x = N.x]. This is essential for
   correctness, since [M.x] and [N.x] differ at runtime. *)

module Different_applications : sig
  law_? p : M.x = N.x
end = struct
  law_? p : M.x = M.x
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   law_? p : M.x = M.x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig law_? p : M.x = M.x end
       is not included in
         sig law_? p : M.x = N.x end
       Laws do not match:
         law_? p : M.x = M.x
       is not included in
         law_? p : M.x = N.x
       The clauses of the two laws differ: "M.x = M.x" is not the same as
       "M.x = N.x".
|}]

(* Opening an application in a signature gives its values paths through
   the application, which a law cannot refer to. *)

module type Signature_open = sig
  open F (A)
  law_? p : x = x
end
[%%expect {|
Uncaught exception: Not_found

|}]

(* In a signature, a law can in principle be written using functor applications
   (since functor applications can sneak back in via substitutions). See the
   [Substitution_different] test for how this affects implementations. *)

module type Substitution_decl = sig
  module U := F (A)
  law_? p : U.x = U.x
end
[%%expect {|
module type Substitution_decl = sig law_? p : F(A).x = F(A).x end
|}]

(* Likewise with a [with module M := F (A)] constraint. *)

module type With_value = sig
  module M : sig val x : int end
  law_? p : M.x = M.x
end
module type With_applied = With_value with module M := F (A)
[%%expect {|
module type With_value =
  sig module M : sig val x : int end law_? p : M.x = M.x end
module type With_applied = sig law_? p : F(A).x = F(A).x end
|}]

(* However, in implementations we again make sure to not unify value paths such
   as [F(A).x] with [M.x] and [N.x]. That is, for correctness, we must prevent
   the chain [M.x =u= F(A).x =u= N.x], where [=u=] denotes unifiability. *)

module Substitution_different : Substitution_decl = struct
  law_? p : M.x = N.x
end
[%%expect {|
Lines 1-3, characters 52-3:
1 | ....................................................struct
2 |   law_? p : M.x = N.x
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig law_? p : M.x = N.x end
       is not included in
         Substitution_decl
       Laws do not match:
         law_? p : M.x = N.x
       is not included in
         law_? p : F(A).x = F(A).x
       The clauses of the two laws differ: "M.x = N.x" is not the same as
       "F(A).x = F(A).x".
|}]

(* As a consequence, a structure cannot satisfy such a law by binding
   [F (A)] to a module of its own, even if it states the same law. *)

module Substitution_decl_sig : sig
  module M := F (A)
  law_? p : M.x = M.x
end = struct
  module Q = F (A)
  law_? p : Q.x = Q.x
end
[%%expect {|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   module Q = F (A)
6 |   law_? p : Q.x = Q.x
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           module Q : sig type t = F(A).t = A | B val x : int end
           law_? p : Q.x = Q.x
         end
       is not included in
         sig law_? p : F(A).x = F(A).x end
       Laws do not match:
         law_? p : Q.x = Q.x
       is not included in
         law_? p : F(A).x = F(A).x
       The clauses of the two laws differ: "Q.x = Q.x" is not the same as
       "F(A).x = F(A).x".
|}]

(* Applying a functor to the result of another functor application can yield
   laws with functor applications in the path. *)

module Claim (X : sig val x : int end) = struct
  law_? p : X.x = X.x
end
module Applied_claim = Claim (F (A))
[%%expect {|
module Claim :
  functor (X : sig val x : int end) -> sig law_? p : X.x = X.x end
module Applied_claim : sig law_? p : F(A).x = F(A).x end
|}]

(* They still don't unify with other applications of [F] such as [M]. *)

module Ascribed : sig law_? p : M.x = M.x end = Claim (F (A))
[%%expect {|
Line 1, characters 48-61:
1 | module Ascribed : sig law_? p : M.x = M.x end = Claim (F (A))
                                                    ^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig law_? p : F(A).x = F(A).x end
       is not included in
         sig law_? p : M.x = M.x end
       Laws do not match:
         law_? p : F(A).x = F(A).x
       is not included in
         law_? p : M.x = M.x
       The clauses of the two laws differ: "F(A).x = F(A).x" is not the same as
       "M.x = M.x".
|}]
