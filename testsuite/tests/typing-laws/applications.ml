(* TEST
 flags = "-extension laws";
 expect;
*)

(* Each functor application results in a new module. Thus, a value
   referred to through an application, as [F(A).y], is not the value of an
   individual module: two applications of [F] to [A] have different
   values. A law that refers to such a value is rejected where laws are
   consumed, that is, by module inclusion. *)

module F (X : sig val r : int ref end) = struct
  type t = A | B
  let y = incr X.r; !X.r
  exception E
end
module A = struct let r = ref 0 end
module M = F (A)
module N = F (A)
let _ = (M.y, N.y)
[%%expect {|
module F :
  functor (X : sig val r : int ref end) ->
    sig type t = A | B val y : int exception E end
module A : sig val r : int ref end
module M : sig type t = F(A).t = A | B val y : int exception E end
module N : sig type t = F(A).t = A | B val y : int exception E end
- : int * int = (1, 2)
|}]

(* We cannot unify the value paths of functor applications. As the example above
   shows, [M] and [N] are different modules: [M.y] is [1] where [N.y] is [2].
   Thus, we must ensure that [M.y = M.y] does not satisfy [M.y = N.y]. *)

module Different_applications : sig
  law? p : M.y = N.y
end = struct
  law? p : M.y = M.y
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   law? p : M.y = M.y
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig law? p : M.y = M.y end
       is not included in
         sig law? p : M.y = N.y end
       Laws do not match:
         law? p : M.y = M.y
       is not included in
         law? p : M.y = N.y
       The clauses of the two laws differ.
|}]

(* Opening an application in a signature gives its values paths through
   the application, which a law cannot refer to. *)

module type Signature_open = sig
  open F (A)
  law? p : y = y
end
[%%expect {|
Line 3, characters 11-12:
3 |   law? p : y = y
               ^
Error: Laws do not support values through functor applications.
|}]

(* A module substitution of an application keeps the functor application
   in module paths. As such, the below fails but [module M = F (A)] would
   work. *)

module type Substitution_decl = sig
  module M := F (A)
  law? p : M.y = M.y
end
[%%expect {|
Line 1:
Error: Module type declarations do not match:
         module type Substitution_decl = sig law? p : F(A).y = F(A).y end
       does not match
         module type Substitution_decl = sig law? p : F(A).y = F(A).y end
       At position "module type Substitution_decl = <here>"
       Module types do not match:
         sig law? p : F(A).y = F(A).y end
       is not equal to
         sig law? p : F(A).y = F(A).y end
       At position "module type Substitution_decl = <here>"
       Laws do not match:
         law? p : F(A).y = F(A).y
       is not included in
         law? p : F(A).y = F(A).y
       The law refers to "F(A).y". Each functor application results in a new
       module, so each application of "F" to "A" is a new module with its own "y".
|}]

module Substitution_decl_sig : sig
  module M := F (A)
  law? p : M.y = M.y
end = struct
  module M : sig val y : int end = F (A)
  law? p : M.y = M.y
end
[%%expect {|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   module M : sig val y : int end = F (A)
6 |   law? p : M.y = M.y
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig module M : sig val y : int end law? p : M.y = M.y end
       is not included in
         sig law? p : F(A).y = F(A).y end
       Laws do not match:
         law? p : M.y = M.y
       is not included in
         law? p : F(A).y = F(A).y
       The law refers to "F(A).y". Each functor application results in a new
       module, so each application of "F" to "A" is a new module with its own "y".
|}]

(* Likewise with a [with module M := F (A)] constraint. *)

module type With_value = sig
  module M : sig val y : int end
  law? p : M.y = M.y
end
[%%expect {|
module type With_value =
  sig module M : sig val y : int end law? p : M.y = M.y end
|}]

module type With_applied = With_value with module M := F (A)
[%%expect {|
Line 1:
Error: Module type declarations do not match:
         module type With_applied = sig law? p : F(A).y = F(A).y end
       does not match
         module type With_applied = sig law? p : F(A).y = F(A).y end
       At position "module type With_applied = <here>"
       Module types do not match:
         sig law? p : F(A).y = F(A).y end
       is not equal to
         sig law? p : F(A).y = F(A).y end
       At position "module type With_applied = <here>"
       Laws do not match:
         law? p : F(A).y = F(A).y
       is not included in
         law? p : F(A).y = F(A).y
       The law refers to "F(A).y". Each functor application results in a new
       module, so each application of "F" to "A" is a new module with its own "y".
|}]

(* Applying a functor to an application gives the laws of the result such
   paths too. *)

module Claim (X : sig val y : int end) = struct
  law? p : X.y = X.y
end
[%%expect {|
module Claim :
  functor (X : sig val y : int end) -> sig law? p : X.y = X.y end
|}]

module Applied_claim = Claim (F (A))
[%%expect {|
Line 1:
Error: In module "Applied_claim":
       Modules do not match:
         sig law? p : F(A).y = F(A).y end
       is not included in
         sig law? p : F(A).y = F(A).y end
       In module "Applied_claim":
       Laws do not match:
         law? p : F(A).y = F(A).y
       is not included in
         law? p : F(A).y = F(A).y
       The law refers to "F(A).y". Each functor application results in a new
       module, so each application of "F" to "A" is a new module with its own "y".
|}]

module Ascribed : sig law? p : M.y = M.y end = Claim (F (A))
[%%expect {|
Line 1, characters 47-60:
1 | module Ascribed : sig law? p : M.y = M.y end = Claim (F (A))
                                                   ^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig law? p : F(A).y = F(A).y end
       is not included in
         sig law? p : M.y = M.y end
       Laws do not match:
         law? p : F(A).y = F(A).y
       is not included in
         law? p : M.y = M.y
       The law refers to "F(A).y". Each functor application results in a new
       module, so each application of "F" to "A" is a new module with its own "y".
|}]

(* Binding the application to a module first gives its values a path. *)

module Bound = F (A)
module type Substitution_bound = sig
  module M := Bound
  law? p : M.y = M.y
end
module Bound_claim = Claim (Bound)
[%%expect {|
module Bound : sig type t = F(A).t = A | B val y : int exception E end
module type Substitution_bound = sig law? p : Bound.y = Bound.y end
module Bound_claim : sig law? p : Bound.y = Bound.y end
|}]
