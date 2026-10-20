(* TEST
 flags = "-extension laws -extension module_strengthening";
 expect;
*)

(* The paths of a clause are compared up to module aliases: [Alias.f] and
   [Outer.f] are the same value. *)

module Outer = struct
  let f x = x
end
module Alias = Outer
module Through_alias : sig
  law? l (x : int) : Outer.f x = x
end = struct
  law? l (x : int) : Alias.f x = x
end
[%%expect {|
module Outer : sig val f : 'a -> 'a end
module Alias = Outer
module Through_alias : sig law? l (x : int) : (Outer.f x) = x end
|}]

(* A law is about particular values, not about their types: [A] and [B]
   have the same signature, but [A.x = 0] holds where [B.x = 0] does not.
   Two paths are the same value only if they lead to the same module. *)

module type Int = sig val x : int end
module A : Int = struct let x = 0 end
module B : Int = struct let x = 1 end
module Different_modules : sig
  law? same : A.x = 0
end = struct
  law? same : B.x = 0
end
[%%expect {|
module type Int = sig val x : int end
module A : Int
module B : Int
Lines 6-8, characters 6-3:
6 | ......struct
7 |   law? same : B.x = 0
8 | end
Error: Signature mismatch:
       Modules do not match:
         sig law? same : B.x = (0) end
       is not included in
         sig law? same : A.x = (0) end
       Laws do not match:
         law? same : B.x = (0)
       is not included in
         law? same : A.x = (0)
       The clauses of the two laws differ.
|}]

(* A module constrained by a signature is not known to be the original
   module, as for its abstract types: [Outer'.f] is not [Outer.f]. *)

module Outer' : sig val f : int -> int end = Outer
module Constrained : sig
  law? l (x : int) : Outer.f x = x
end = struct
  law? l (x : int) : Outer'.f x = x
end
[%%expect {|
module Outer' : sig val f : int -> int end
Lines 4-6, characters 6-3:
4 | ......struct
5 |   law? l (x : int) : Outer'.f x = x
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig law? l (x : int) : (Outer'.f x) = x end
       is not included in
         sig law? l (x : int) : (Outer.f x) = x end
       Laws do not match:
         law? l (x : int) : (Outer'.f x) = x
       is not included in
         law? l (x : int) : (Outer.f x) = x
       The clauses of the two laws differ.
|}]

(* Once a module is in the environment, the clauses of its laws refer to its
   values through its path: the law of [M.N], looked up through [M], refers to
   [M.f], whereas the law of a signature refers to [f] by name. When nothing
   relates the two (e.g., no outer signature), the laws of a submodule that
   refer to the values of the enclosing module fail typechecking. *)

module M = struct
  let f x = x
  module N = struct
    law? l (x : int) : f x = x
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
           module N : sig law? l (x : int) : (f x) = x end
         end
       In module "M.N":
       Modules do not match:
         sig law? l (x : int) : (M.f x) = x end
       is not included in
         sig law? l (x : int) : (f x) = x end
       In module "M.N":
       Laws do not match:
         law? l (x : int) : (M.f x) = x
       is not included in
         law? l (x : int) : (f x) = x
       The first law refers to "M.f", but the second law refers to "f". The two
       are not known to be the same value.
       Hint: consider referring to "M.f" in the second one.
|}]

(* A signature suffices to locate [f] as far as [l] is concerned. *)

module type T = sig
  val f : int -> int
  module N : sig
    law? l (x : int) : f x = x
  end
end
module M7 : T = struct
  let f x = x
  module N = struct
    law? l (x : int) : f x = x
  end
end
[%%expect {|
module type T =
  sig val f : int -> int module N : sig law? l (x : int) : (f x) = x end end
module M7 : T
|}]

(* The laws of a module that is compared as a whole refer to its values by
   name, as the laws of the signature do: a module path given as a functor
   argument matches. *)

module type Idem = sig
  val f : int -> int
  law? idem (x : int) : f (f x) = f x
end
module X_idem = struct
  let f x = x
  law? idem (x : int) : f (f x) = f x
end
module F_idem (X : Idem) = struct end
module Applied = F_idem (X_idem)
[%%expect {|
module type Idem =
  sig val f : int -> int law? idem (x : int) : (f (f x)) = (f x) end
module X_idem :
  sig val f : 'a -> 'a law? idem (x : int) : (f (f x)) = (f x) end
module F_idem : functor (X : Idem) -> sig end
module Applied : sig end
|}]


(* A law about the [f] of another module is not the law of the signature,
   which is about the [f] of the structure itself. *)

module Other = struct let f x = x end
module Not_own = struct
  let f x = x
  law? idem (x : int) : Other.f (Other.f x) = Other.f x
end
let not_own = (module Not_own : Idem)
[%%expect {|
module Other : sig val f : 'a -> 'a end
module Not_own :
  sig
    val f : 'a -> 'a
    law? idem (x : int) : (Other.f (Other.f x)) = (Other.f x)
  end
Line 6, characters 22-29:
6 | let not_own = (module Not_own : Idem)
                          ^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig
           val f : 'a -> 'a
           law? idem (x : int) : (Other.f (Other.f x)) = (Other.f x)
         end
       is not included in
         Idem
       Laws do not match:
         law? idem (x : int) : (Other.f (Other.f x)) = (Other.f x)
       is not included in
         law? idem (x : int) : (f (f x)) = (f x)
       The first law refers to "Other.f", but the second law refers to "f". The
       two are not known to be the same value.
       Hint: consider referring to "Other.f" in the second one.
|}]
