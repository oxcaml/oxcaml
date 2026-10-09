(* TEST
 flags = "-extension laws";
 expect;
*)

(* A functor application names no particular instance of the functor: the
   values of two applications are never equated, and a law that refers to
   a value through an application is rejected where laws are consumed. *)

module F (X : sig end) = struct
  type t = A | B
  let x = ref 0
  let y = 0
  exception E
end
module A = struct end
module M = F (A)
module N = F (A)
[%%expect {|
module F :
  functor (X : sig end) ->
    sig type t = A | B val x : int ref val y : int exception E end
module A : sig end
module M :
  sig type t = F(A).t = A | B val x : int ref val y : int exception E end
module N :
  sig type t = F(A).t = A | B val x : int ref val y : int exception E end
|}]

(* [M.x] and [N.x] are different values: [M.x == M.x] does not satisfy
   [M.x == N.x]. *)

module Different_instances : sig
  law? p : M.x == N.x
end = struct
  law? p : M.x == M.x
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   law? p : M.x == M.x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig law? p : M.x == M.x end
       is not included in
         sig law? p : M.x == N.x end
       Laws do not match:
         law? p : M.x == M.x
       is not included in
         law? p : M.x == N.x
       The clauses of the laws differ.
|}]

(* Nor are [M.E] and [N.E] the same exception. *)

module Different_exceptions : sig
  law? e : M.E = N.E
end = struct
  law? e : M.E = M.E
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   law? e : M.E = M.E
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig law? e : M.E = M.E end
       is not included in
         sig law? e : M.E = N.E end
       Laws do not match:
         law? e : M.E = M.E
       is not included in
         law? e : M.E = N.E
       The clauses of the laws differ.
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

(* So does substituting a local module bound to an application. *)

law? local : (let module L = F (A) in L.x == L.x)
[%%expect {|
Line 1, characters 13-49:
1 | law? local : (let module L = F (A) in L.x == L.x)
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Laws do not support values through functor applications.
|}]

(* A module substitution of an application gives the laws that refer to
   the values of the module such paths. The toplevel checks the signature
   of each phrase against itself, as the compiler does an interface and
   the inferred signature of an implementation, and reports such a law
   there. *)

module type Substitution_decl = sig
  module M := F (A)
  law? p : M.x == M.x
end
[%%expect {|
Line 1:
Error: Module type declarations do not match:
         module type Substitution_decl = sig law? p : F(A).x == F(A).x end
       does not match
         module type Substitution_decl = sig law? p : F(A).x == F(A).x end
       At position "module type Substitution_decl = <here>"
       Module types do not match:
         sig law? p : F(A).x == F(A).x end
       is not equal to
         sig law? p : F(A).x == F(A).x end
       At position "module type Substitution_decl = <here>"
       Laws do not match:
         law? p : F(A).x == F(A).x
       is not included in
         law? p : F(A).x == F(A).x
       The law refers to "F(A).x" through a functor application, which names no
       particular instance.
|}]

(* Where the module type does not reach a signature, the law is reported
   when a module is checked against it. *)

module Checked : sig
  module M := F (A)
  law? p : M.x == M.x
end = struct
  module M : sig val x : int ref end = F (A)
  law? p : M.x == M.x
end
[%%expect {|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   module M : sig val x : int ref end = F (A)
6 |   law? p : M.x == M.x
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig module M : sig val x : int ref end law? p : M.x == M.x end
       is not included in
         sig law? p : F(A).x == F(A).x end
       Laws do not match:
         law? p : M.x == M.x
       is not included in
         law? p : F(A).x == F(A).x
       The law refers to "F(A).x" through a functor application, which names no
       particular instance.
|}]

(* Likewise with a [with module M := F (A)] constraint. *)

module type With_value = sig
  module M : sig val x : int ref end
  law? p : M.x == M.x
end
[%%expect {|
module type With_value =
  sig module M : sig val x : int ref end law? p : M.x == M.x end
|}]

module type With_applied = With_value with module M := F (A)
[%%expect {|
Line 1:
Error: Module type declarations do not match:
         module type With_applied = sig law? p : F(A).x == F(A).x end
       does not match
         module type With_applied = sig law? p : F(A).x == F(A).x end
       At position "module type With_applied = <here>"
       Module types do not match:
         sig law? p : F(A).x == F(A).x end
       is not equal to
         sig law? p : F(A).x == F(A).x end
       At position "module type With_applied = <here>"
       Laws do not match:
         law? p : F(A).x == F(A).x
       is not included in
         law? p : F(A).x == F(A).x
       The law refers to "F(A).x" through a functor application, which names no
       particular instance.
|}]

module With_checked : With_value with module M := F (A) = struct
  law? p : M.x == M.x
end
[%%expect {|
Lines 1-3, characters 58-3:
1 | ..........................................................struct
2 |   law? p : M.x == M.x
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig law? p : M.x == M.x end
       is not included in
         sig law? p : F(A).x == F(A).x end
       Laws do not match:
         law? p : M.x == M.x
       is not included in
         law? p : F(A).x == F(A).x
       The law refers to "F(A).x" through a functor application, which names no
       particular instance.
|}]

(* A constraint that substitutes only types is fine. *)

module type With_type = sig
  module M : sig type t = A | B end
  law? p (x : M.t) : (match x with M.A -> true | M.B -> false)
end
module type Accepted = With_type with module M := F (A)
[%%expect {|
module type With_type =
  sig
    module M : sig type t = A | B end
    law? p (x : M.t) :
      (match x with | (M.A : M.t) -> true | (M.B : M.t) -> false)
  end
module type Accepted =
  sig
    law? p (x : F(A).t) :
      (match x with | (A : F(A).t) -> true | (B : F(A).t) -> false)
  end
|}]

(* The printed law types again, the constructors being resolved by their
   type annotations. *)

module Printed : Accepted = struct
  law? p (x : F(A).t) :
    (match x with | (A : F(A).t) -> true | (B : F(A).t) -> false)
end
[%%expect {|
module Printed : Accepted
|}]

(* Applying a functor to an application gives the laws of the result such
   paths too. *)

module Claim (X : sig val x : int ref end) = struct
  law? p : X.x == X.x
end
[%%expect {|
module Claim :
  functor (X : sig val x : int ref end) -> sig law? p : X.x == X.x end
|}]

module Applied_claim = Claim (F (A))
[%%expect {|
Line 1:
Error: In module "Applied_claim":
       Modules do not match:
         sig law? p : F(A).x == F(A).x end
       is not included in
         sig law? p : F(A).x == F(A).x end
       In module "Applied_claim":
       Laws do not match:
         law? p : F(A).x == F(A).x
       is not included in
         law? p : F(A).x == F(A).x
       The law refers to "F(A).x" through a functor application, which names no
       particular instance.
|}]

module Ascribed : sig law? p : M.x == M.x end = Claim (F (A))
[%%expect {|
Line 1, characters 48-61:
1 | module Ascribed : sig law? p : M.x == M.x end = Claim (F (A))
                                                    ^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig law? p : F(A).x == F(A).x end
       is not included in
         sig law? p : M.x == M.x end
       Laws do not match:
         law? p : F(A).x == F(A).x
       is not included in
         law? p : M.x == M.x
       The law refers to "F(A).x" through a functor application, which names no
       particular instance.
|}]

(* So does the expansion of the module type of an application, when a
   module is checked against it. *)

module type V = sig val x : int ref end
module Behind (X : V) = struct
  module type T = sig law? p : X.x == X.x end
end
module type S = sig
  module M : V
  module N : Behind (M).T
end
module type Bad = S with module M := F (A)
module G (X : sig end) = struct end
module type Nested = sig module N : Behind (F (G (A))).T end
[%%expect {|
module type V = sig val x : int ref end
module Behind :
  functor (X : V) -> sig module type T = sig law? p : X.x == X.x end end
module type S = sig module M : V module N : Behind(M).T end
module type Bad = sig module N : Behind(F(A)).T end
module G : functor (X : sig end) -> sig end
module type Nested = sig module N : Behind(F(G(A))).T end
|}]

module B : Behind (F (A)).T = struct law? p : true end
[%%expect {|
Line 1, characters 30-54:
1 | module B : Behind (F (A)).T = struct law? p : true end
                                  ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig law? p : true end
       is not included in
         Behind(F(A)).T
       Laws do not match:
         law? p : true
       is not included in
         law? p : F(A).x == F(A).x
       The law refers to "F(A).x" through a functor application, which names no
       particular instance.
|}]

(* The types and module types of an application can be referred to, as
   nothing consumes its laws. The argument of the application is still
   checked against the parameter, laws included. *)

module Claim_types (X : sig val x : int ref end) = struct
  type t = int
  module type S = sig type u end
  module Sub = struct type v law? r : X.x == X.x end
  law? p : X.x == X.x
end
type u = Claim_types (F (A)).t
module type T = Claim_types (F (A)).S
type w = Claim_types (F (A)).Sub.v
module Lawful (X : sig end) = struct
  let y = 0
  law? l : y = y
end
module Requires (X : sig val y : int law? l : y = y end) = struct type t end
type checked = Requires (Lawful (A)).t
[%%expect {|
module Claim_types :
  functor (X : sig val x : int ref end) ->
    sig
      type t = int
      module type S = sig type u end
      module Sub : sig type v law? r : X.x == X.x end
      law? p : X.x == X.x
    end
type u = Claim_types(F(A)).t
module type T = Claim_types(F(A)).S
type w = Claim_types(F(A)).Sub.v
module Lawful : functor (X : sig end) -> sig val y : int law? l : y = y end
module Requires :
  functor (X : sig val y : int law? l : y = y end) -> sig type t end
type checked = Requires(Lawful(A)).t
|}]

(* Binding the application to a module first gives its values a path. *)

module Bound = F (A)
module type Substitution_bound = sig
  module M := Bound
  law? p : M.x == M.x
end
module Bound_claim = Claim (Bound)
module type Open_bound = sig
  open Bound
  law? p : y = y
end
module type Good = With_value with module M := Bound
[%%expect {|
module Bound :
  sig type t = F(A).t = A | B val x : int ref val y : int exception E end
module type Substitution_bound = sig law? p : Bound.x == Bound.x end
module Bound_claim : sig law? p : Bound.x == Bound.x end
module type Open_bound = sig law? p : Bound.y = Bound.y end
module type Good = sig law? p : Bound.x == Bound.x end
|}]

(* The values of a functor returning its parameter are those of the
   argument, when the argument is a bound module: applied to an
   application, the result's values are those of no instance. *)

module Id (X : sig val x : int ref exception E end) = X
module Id_bound = Id (Bound)
module Id_bound' = Id (Bound)
module Same_instance : sig
  law? p : Id_bound.x == Id_bound'.x
  law? q : Id_bound.x == Bound.x
  law? e : Id_bound.E = Id_bound'.E
end = struct
  law? p : Bound.x == Bound.x
  law? q : Bound.x == Bound.x
  law? e : Bound.E = Bound.E
end
[%%expect {|
module Id :
  functor (X : sig val x : int ref exception E end) ->
    sig val x : int ref exception E end
module Id_bound : sig val x : int ref exception E end
module Id_bound' : sig val x : int ref exception E end
module Same_instance :
  sig
    law? p : Id_bound.x == Id_bound'.x
    law? q : Id_bound.x == Bound.x
    law? e : Id_bound.E = Id_bound'.E
  end
|}]

module Id_applied = Id (F (A))
module Id_applied' = Id (F (A))
module Different_results : sig
  law? p : Id_applied.x == Id_applied'.x
end = struct
  law? p : Id_applied.x == Id_applied.x
end
[%%expect {|
module Id_applied : sig val x : int ref exception E end
module Id_applied' : sig val x : int ref exception E end
Lines 5-7, characters 6-3:
5 | ......struct
6 |   law? p : Id_applied.x == Id_applied.x
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig law? p : Id_applied.x == Id_applied.x end
       is not included in
         sig law? p : Id_applied.x == Id_applied'.x end
       Laws do not match:
         law? p : Id_applied.x == Id_applied.x
       is not included in
         law? p : Id_applied.x == Id_applied'.x
       The clauses of the laws differ.
|}]

module Different_result_exceptions : sig
  law? e : Id_applied.E = Id_applied'.E
end = struct
  law? e : Id_applied.E = Id_applied.E
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   law? e : Id_applied.E = Id_applied.E
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig law? e : Id_applied.E = Id_applied.E end
       is not included in
         sig law? e : Id_applied.E = Id_applied'.E end
       Laws do not match:
         law? e : Id_applied.E = Id_applied.E
       is not included in
         law? e : Id_applied.E = Id_applied'.E
       The clauses of the laws differ.
|}]

(* The laws of a submodule of an application are checked through a module
   bound to the application. *)

module type Idem = sig
  val f : int -> int
  law? idem (x : int) : f (f x) = f x
end
module G (X : sig end) = struct
  module M = struct
    let f x = x
    law? idem (x : int) : f (f x) = f x
  end
end
module GY = G (struct end)
module Z : Idem = GY.M
[%%expect {|
module type Idem =
  sig val f : int -> int law? idem (x : int) : (f (f x)) = (f x) end
module G :
  functor (X : sig end) ->
    sig
      module M :
        sig val f : 'a -> 'a law? idem (x : int) : (f (f x)) = (f x) end
    end
module GY :
  sig
    module M :
      sig val f : 'a -> 'a law? idem (x : int) : (f (f x)) = (f x) end
  end
module Z : Idem
|}]

(* A functor whose body is a module path returns that module: the values
   of the result are those of the path. *)

module X_idem = struct
  let f x = x
  law? idem (x : int) : f (f x) = f x
end
module Generative () = X_idem
module Generated = Generative ()
module Through_result : sig
  law? l (x : int) : Generated.f x = x
end = struct
  law? l (x : int) : X_idem.f x = x
end
[%%expect {|
module X_idem :
  sig val f : 'a -> 'a law? idem (x : int) : (f (f x)) = (f x) end
module Generative :
  functor () ->
    sig val f : 'a -> 'a law? idem (x : int) : (f (f x)) = (f x) end
module Generated :
  sig val f : 'a -> 'a law? idem (x : int) : (f (f x)) = (f x) end
module Through_result : sig law? l (x : int) : (Generated.f x) = x end
|}]

(* An alias of an application is the application. *)

module Make (X : sig end) = struct
  let f x = x
  law? idem (x : int) : f (f x) = f x
end
module Int_map = Make (struct end)
module IM = Int_map
module Through_alias : sig
  law? l (x : int) : IM.f x = x
end = struct
  law? l (x : int) : Int_map.f x = x
end
[%%expect {|
module Make :
  functor (X : sig end) ->
    sig val f : 'a -> 'a law? idem (x : int) : (f (f x)) = (f x) end
module Int_map :
  sig val f : 'a -> 'a law? idem (x : int) : (f (f x)) = (f x) end
module IM = Int_map
module Through_alias : sig law? l (x : int) : (IM.f x) = x end
|}]

(* The laws of the body of an applicative functor are checked against its
   result signature as a structure against a signature. *)

module H (X : sig end) : Idem = struct
  let f x = x
  law? idem (x : int) : f (f x) = f x
end
[%%expect {|
module H : functor (X : sig end) -> Idem
|}]
