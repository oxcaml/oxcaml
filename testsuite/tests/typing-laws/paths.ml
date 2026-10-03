(* TEST
 flags = "-extension laws -extension module_strengthening";
 expect;
*)

(* The paths of a clause are compared up to module aliases: [Alias.f] and
   [Outer.f] are the same value, as are [Alias.E] and [Outer.E]. *)

module Outer = struct
  let f x = x
  exception E of int
end
module type Outer_sig = sig
  val f : int -> int
  exception E of int
end
module Outer' : Outer_sig = Outer
module Alias = Outer
module Through_alias : sig
  law? l (x : int) :
    Outer.f x = x && (try raise (Outer.E x) with Outer.E y -> y = x)
end = struct
  law? l (x : int) :
    Alias.f x = x && (try raise (Alias.E x) with Alias.E y -> y = x)
end
[%%expect {|
module Outer : sig val f : 'a -> 'a exception E of int end
module type Outer_sig = sig val f : int -> int exception E of int end
module Outer' : Outer_sig
module Alias = Outer
module Through_alias :
  sig
    law? l (x : int) :
      ((Outer.f x) = x) &&
        ((try raise (Outer.E x) with | Outer.E y -> y = x))
  end
|}]

(* Paths are compared as the paths of types are, not through the
   declarations they lead to: the values of two modules with the same
   signature are different values. *)

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
         sig law? same : B.x = 0 end
       is not included in
         sig law? same : A.x = 0 end
       Laws do not match:
         law? same : B.x = 0
       is not included in
         law? same : A.x = 0
       The clauses of the laws differ.
|}]

(* A module constrained by a signature is not known to be the original
   module, as for its abstract types: [Outer'.f] is not [Outer.f]. *)

module Constrained : sig
  law? l (x : int) : Outer.f x = x
end = struct
  law? l (x : int) : Outer'.f x = x
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   law? l (x : int) : Outer'.f x = x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig law? l (x : int) : (Outer'.f x) = x end
       is not included in
         sig law? l (x : int) : (Outer.f x) = x end
       Laws do not match:
         law? l (x : int) : (Outer'.f x) = x
       is not included in
         law? l (x : int) : (Outer.f x) = x
       The clauses of the laws differ.
|}]

(* Once a module is in the environment, the clauses of its laws refer to
   its values through its path: the law of [M.N], looked up through [M],
   refers to [M.f], whereas the law of a signature refers to [f] by name.
   Nothing relates the two, so the laws of a submodule that refer to the
   values of the enclosing module cannot be compared, not even with the
   signature inferred for the module. *)

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
       The first refers to "M.f" where the second refers to "f". The laws of a
       module referred to by a path cannot be compared with the laws of a
       signature.
|}]

(* A structure is not looked up through a path: its laws, including those
   of its submodules, are compared with those of the signature. *)

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
   argument matches, with or without a constraint. *)

module type Idem = sig
  val f : int -> int
  law? idem (x : int) : f (f x) = f x
end
module X_idem = struct
  let f x = x
  law? idem (x : int) : f (f x) = f x
end
module F_idem (X : sig
    val f : int -> int
    law? idem (x : int) : f (f x) = f x
  end) = struct end
module Applied = F_idem (X_idem)
module Applied_constrained = F_idem ((X_idem : Idem))
[%%expect {|
module type Idem =
  sig val f : int -> int law? idem (x : int) : (f (f x)) = (f x) end
module X_idem :
  sig val f : 'a -> 'a law? idem (x : int) : (f (f x)) = (f x) end
module F_idem :
  functor
    (X : sig val f : int -> int law? idem (x : int) : (f (f x)) = (f x) end)
    -> sig end
module Applied : sig end
module Applied_constrained : sig end
|}]

(* And wherever the type of a module path is checked against a signature:
   packing a first-class module (through a nested path, an alias, a local
   module, an unpacked module), [module type of], a strengthened
   signature, a functor returning its parameter, recursive modules, and
   exceptions in clauses. *)

module Packed = struct
  let p = (module X_idem : Idem)
  module Outer = struct module Inner = X_idem end
  let q = (module Outer.Inner : Idem)
  module Alias = X_idem
  let r = (module Alias : Idem)
  let s = let module M = X_idem in (module M : Idem)
  module Unpacked = (val p : Idem)
  let t = (module Unpacked : Idem)
  let u (module M : Idem) = (module M : Idem)
end
module Type_of : module type of X_idem = X_idem
module Strengthened : Idem with X_idem = X_idem
let strengthened = (module Strengthened : Idem)
module Returned (X : Idem) : Idem = X
module Returned_applied : Idem = Returned (X_idem)
let returned_applied = (module Returned_applied : Idem)
module Nested_alias (X : Idem) = struct module Y = X end
module Nested_alias_applied = Nested_alias (X_idem)
module Nested_alias_applied' : Idem = Nested_alias_applied.Y
module rec Rec : Idem = X_idem
and Rec_struct : Idem = struct
  let f x = x
  law? idem (x : int) : f (f x) = f x
end
module type Exn = sig exception E law? raised : (try raise E with E -> true) end
module X_exn = struct exception E law? raised : (try raise E with E -> true) end
let exn = (module X_exn : Exn)
[%%expect {|
module Packed :
  sig
    val p : (module Idem)
    module Outer : sig module Inner = X_idem end
    val q : (module Idem)
    module Alias = X_idem
    val r : (module Idem)
    val s : (module Idem)
    module Unpacked : Idem
    val t : (module Idem)
    val u : (module Idem) -> (module Idem)
  end
module Type_of :
  sig
    val f : 'a -> 'a @@ stateless
    law? idem (x : int) : (f (f x)) = (f x)
  end
module Strengthened :
  sig val f : int -> int law? idem (x : int) : (f (f x)) = (f x) end
val strengthened : (module Idem) = <module>
module Returned : functor (X : Idem) -> Idem
module Returned_applied : Idem
val returned_applied : (module Idem) = <module>
module Nested_alias :
  functor (X : Idem) ->
    sig
      module Y :
        sig val f : int -> int law? idem (x : int) : (f (f x)) = (f x) end
    end
module Nested_alias_applied :
  sig
    module Y :
      sig val f : int -> int law? idem (x : int) : (f (f x)) = (f x) end
  end
module Nested_alias_applied' : Idem
module rec Rec : Idem
and Rec_struct : Idem
module type Exn =
  sig exception E law? raised : (try raise E with | E -> true) end
module X_exn :
  sig exception E law? raised : (try raise E with | E -> true) end
val exn : (module Exn) = <module>
|}]

(* Inside a functor, the parameter can be checked against its signature,
   packed and applied; and [with module M = X] identifies the submodule
   with [X]. *)

module In_functor (P : Idem) = struct
  module Q : Idem = P
  let p = (module P : Idem)
  module Applied = F_idem (P)
end
module type With_module = sig module M : Idem end
module With_module : With_module with module M = X_idem = struct
  module M = X_idem
end
module type Without_module = With_module with module M := X_idem
[%%expect {|
module In_functor :
  functor (P : Idem) ->
    sig module Q : Idem val p : (module Idem) module Applied : sig end end
module type With_module = sig module M : Idem end
module With_module :
  sig
    module M :
      sig
        val f : 'a -> 'a @@ stateless
        law? idem (x : int) : (f (f x)) = (f x)
      end
  end
module type Without_module = sig end
|}]

(* A law of the structure that refers to the value [f] of another module
   is not the law of the signature, which refers to the [f] of the
   structure. It is rejected as the laws of a module referred to by a path
   are: only the name tells the two apart. *)

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
       The first refers to "Other.f" where the second refers to "f". The laws of
       a module referred to by a path cannot be compared with the laws of a
       signature.
|}]

(* The laws of a module bound to another module refer to its values by
   name too: the result of a functor whose body is a module path
   (generative or applicative), or a module ascribed to the module type of
   a structure including the module. *)

module Generative () = X_idem
module Generated = Generative ()
let generated = (module Generated : Idem)
module Applicative (E : sig end) = X_idem
module Applied_to_structure = Applicative (struct end)
module Applied_to_structure' : Idem = Applied_to_structure
module type Type_of_included = module type of struct include X_idem end
module Through_type_of : Type_of_included = X_idem
module Through_type_of' : Idem = Through_type_of
[%%expect {|
module Generative :
  functor () ->
    sig
      val f : 'a -> 'a @@ stateless
      law? idem (x : int) : (f (f x)) = (f x)
    end
module Generated :
  sig
    val f : 'a -> 'a @@ stateless
    law? idem (x : int) : (f (f x)) = (f x)
  end
val generated : (module Idem) = <module>
module Applicative :
  functor (E : sig end) ->
    sig
      val f : 'a -> 'a @@ stateless
      law? idem (x : int) : (f (f x)) = (f x)
    end
module Applied_to_structure :
  sig
    val f : 'a -> 'a @@ stateless
    law? idem (x : int) : (f (f x)) = (f x)
  end
module Applied_to_structure' : Idem
module type Type_of_included =
  sig
    val f : 'a -> 'a @@ stateless
    law? idem (x : int) : (f (f x)) = (f x)
  end
module Through_type_of : Type_of_included
module Through_type_of' : Idem
|}]

(* Nothing identifies the values of such modules with those of the
   original: [Generated.f] is not [X_idem.f], nor is [Q.f] [P.f] for an
   alias [Q] of the parameter [P]. A value copied into a structure ([Y.f])
   is a different value. *)

module Through_result : sig
  law? l (x : int) : Generated.f x = x
end = struct
  law? l (x : int) : X_idem.f x = x
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   law? l (x : int) : X_idem.f x = x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig law? l (x : int) : (X_idem.f x) = x end
       is not included in
         sig law? l (x : int) : (Generated.f x) = x end
       Laws do not match:
         law? l (x : int) : (X_idem.f x) = x
       is not included in
         law? l (x : int) : (Generated.f x) = x
       The clauses of the laws differ.
|}]

module Through_parameter (P : Idem) : sig
  law? q (x : int) : P.f x = x
end = struct
  module Q = P
  law? q (x : int) : Q.f x = x
end
[%%expect {|
Lines 3-6, characters 6-3:
3 | ......struct
4 |   module Q = P
5 |   law? q (x : int) : Q.f x = x
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           module Q :
             sig
               val f : int -> int
               law? idem (x : int) : (f (f x)) = (f x)
             end
           law? q (x : int) : (Q.f x) = x
         end
       is not included in
         sig law? q (x : int) : (P.f x) = x end
       Laws do not match:
         law? q (x : int) : (Q.f x) = x
       is not included in
         law? q (x : int) : (P.f x) = x
       The clauses of the laws differ.
|}]

module Not_through_structure : sig
  law? l (x : int) : Generated.f x = x
end = struct
  module Y = struct let f = X_idem.f end
  law? l (x : int) : Y.f x = x
end
[%%expect {|
Lines 3-6, characters 6-3:
3 | ......struct
4 |   module Y = struct let f = X_idem.f end
5 |   law? l (x : int) : Y.f x = x
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           module Y : sig val f : 'a -> 'a end
           law? l (x : int) : (Y.f x) = x
         end
       is not included in
         sig law? l (x : int) : (Generated.f x) = x end
       Laws do not match:
         law? l (x : int) : (Y.f x) = x
       is not included in
         law? l (x : int) : (Generated.f x) = x
       The clauses of the laws differ.
|}]

(* The same for the types of constructors and the extension constructors
   of a clause. *)

module P = struct
  type t = A | B
  exception E
  module N = struct
    law? l : A <> B && (try raise E with E -> true)
  end
end
[%%expect {|
Line 1:
Error: In module "P":
       Modules do not match:
         sig type t = P.t = A | B exception E module N = P.N end
       is not included in
         sig
           type t = A | B
           exception E
           module N :
             sig
               law? l :
                 ((A : t) <> (B : t)) && ((try raise E with | E -> true))
             end
         end
       In module "P.N":
       Modules do not match:
         sig
           law? l :
             ((P.A : P.t) <> (P.B : P.t)) &&
               ((try raise P.E with | P.E -> true))
         end
       is not included in
         sig
           law? l : ((A : t) <> (B : t)) && ((try raise E with | E -> true))
         end
       In module "P.N":
       Laws do not match:
         law? l :
           ((P.A : P.t) <> (P.B : P.t)) &&
             ((try raise P.E with | P.E -> true))
       is not included in
         law? l : ((A : t) <> (B : t)) && ((try raise E with | E -> true))
       The first refers to "P.E" where the second refers to "E". The laws of a
       module referred to by a path cannot be compared with the laws of a
       signature.
|}]
