(* TEST
 flags = "-extension laws";
 expect;
*)

(* This file tests when two paths in a law count as the same value. *)


(* A law is specific modules and their value fields: [A] and [B] have the same
   signature, but [A.x = 0] holds where [B.x = 0] does not. We must not allow
   [A.x] and [B.x] to unify. *)

module type Int = sig val x : int end
module A : Int = struct let x = 0 end
module B : Int = struct let x = 1 end
module Different_modules : sig
  law_? same : A.x = 0
end = struct
  law_? same : B.x = 0
end
[%%expect {|
module type Int = sig val x : int end
module A : Int
module B : Int
Lines 6-8, characters 6-3:
6 | ......struct
7 |   law_? same : B.x = 0
8 | end
Error: Signature mismatch:
       Modules do not match:
         sig law_? same : B.x = (0) end
       is not included in
         sig law_? same : A.x = (0) end
       Laws do not match:
         law_? same : B.x = (0)
       is not included in
         law_? same : A.x = (0)
       The clauses of the two laws differ: "B.x = (0)" is not the same as
       "A.x = (0)".
|}]


(* The paths of a clause are compared up to module aliases, so below [Alias.f]
   and [Outer.f] are the same value. *)

module Outer = struct
  let f x = x
end
module Alias = Outer
module Through_alias : sig
  law_? l (x : int) : Outer.f x = x
end = struct
  law_? l (x : int) : Alias.f x = x
end
[%%expect {|
module Outer : sig val f : 'a -> 'a end
module Alias = Outer
module Through_alias : sig law_? l (x : int) : (Outer.f x) = x end
|}]


(* As for abstract types, abstraction in a signature can cause the type
   system to forget aliasing constraints. Below, the signature hides the
   information that [Outer'.f] is the same as [Outer.f]. *)

module Outer' : sig val f : int -> int end = Outer
module Constrained : sig
  law_? l (x : int) : Outer.f x = x
end = struct
  law_? l (x : int) : Outer'.f x = x
end
[%%expect {|
module Outer' : sig val f : int -> int end
Lines 4-6, characters 6-3:
4 | ......struct
5 |   law_? l (x : int) : Outer'.f x = x
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig law_? l (x : int) : (Outer'.f x) = x end
       is not included in
         sig law_? l (x : int) : (Outer.f x) = x end
       Laws do not match:
         law_? l (x : int) : (Outer'.f x) = x
       is not included in
         law_? l (x : int) : (Outer.f x) = x
       The clauses of the two laws differ: "(Outer'.f x) = x" is not the same
       as "(Outer.f x) = x".
|}]

(* CR sspies: The test below illustrates a limitation of the current
   implementation, not the eventually intended behavior. *)
(* Without an outer signature, referring from an inner module ([N] below) to a
   value from an enclosing module ([M] below) fails the self-inclusion check.
   At the point of the inclusion check, the type system tracks [f] as [M.f],
   but locally inside [N] it is referred to as [f]. *)

module M = struct
  let f x = x
  module N = struct
    law_? l (x : int) : f x = x
  end
end
[%%expect {|
Line 1:
Error: In module "M":
       Modules do not match:
         sig val f : 'a -> 'a module N = M.N end
       is not included in
         sig
           val f : 'a -> 'a
           module N : sig law_? l (x : int) : (f x) = x end
         end
       In module "M.N":
       Modules do not match:
         sig law_? l (x : int) : (M.f x) = x end
       is not included in
         sig law_? l (x : int) : (f x) = x end
       In module "M.N":
       Laws do not match:
         law_? l (x : int) : (M.f x) = x
       is not included in
         law_? l (x : int) : (f x) = x
       The first law refers to "M.f", but the second law refers to "f". The two
       are not known to be the same value.
       Hint: consider referring to "M.f" in the second one.
|}]

(* With a signature for the enclosing module, the law of [N] refers to the [f]
   of that signature, and the module matches. *)

module type T = sig
  val f : int -> int
  module N : sig
    law_? l (x : int) : f x = x
  end
end
module M' : T = struct
  let f x = x
  module N = struct
    law_? l (x : int) : f x = x
  end
end
[%%expect {|
module type T =
  sig val f : int -> int module N : sig law_? l (x : int) : (f x) = x end end
module M' : T
|}]

(* [Idem] requires a law about the module's own [f]. [Not_own] states the
   law about [Other.f] instead, so it does not match [Idem]. *)

module type Idem = sig
  val f : int -> int
  law_? idem (x : int) : f (f x) = f x
end
module Other = struct let f x = x end
module Not_own = struct
  let f x = x
  law_? idem (x : int) : Other.f (Other.f x) = Other.f x
end
module Not_own_checked : Idem = Not_own
[%%expect {|
module type Idem =
  sig val f : int -> int law_? idem (x : int) : (f (f x)) = (f x) end
module Other : sig val f : 'a -> 'a end
module Not_own :
  sig
    val f : 'a -> 'a
    law_? idem (x : int) : (Other.f (Other.f x)) = (Other.f x)
  end
Line 10, characters 32-39:
10 | module Not_own_checked : Idem = Not_own
                                     ^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig
           val f : 'a -> 'a
           law_? idem (x : int) : (Other.f (Other.f x)) = (Other.f x)
         end
       is not included in
         Idem
       Laws do not match:
         law_? idem (x : int) : (Other.f (Other.f x)) = (Other.f x)
       is not included in
         law_? idem (x : int) : (f (f x)) = (f x)
       The first law refers to "Other.f", but the second law refers to "f". The
       two are not known to be the same value.
       Hint: consider referring to "Other.f" in the second one.
|}]
