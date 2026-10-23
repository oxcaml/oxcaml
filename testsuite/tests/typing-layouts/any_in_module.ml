(* TEST
 flags = "-extension layouts_alpha -extension layout_poly_alpha";
 expect;
*)

(* Any in modules: we support signatures whose values have types with
   unrepresentable layouts. We enforce representability when modules are
   constructed or accessed. *)

module type S = sig
  type t : any
  val x : t
  val y : int
end
[%%expect{|
module type S = sig type t : any val x : t val y : int end
|}]

(* We can't project out of a module without knowing its representation. *)
module M : S = struct
  type t = int
  let x = 42
  let y = 43
end

let x = M.x
[%%expect{|
module M : S
Line 7, characters 8-11:
7 | let x = M.x
            ^^^
Error: This expression projects from a module that contains unrepresentable items.
Line 3, characters 2-11:
3 |   val x : t
      ^^^^^^^^^
  "x" is not representable.
  The layout of t is any
    because of the definition of t at line 2, characters 2-14.
  But the layout of t must be representable
    because it's the type of an item in a module being projected from.
|}]

let y = M.y
[%%expect{|
Line 1, characters 8-11:
1 | let y = M.y
            ^^^
Error: This expression projects from a module that contains unrepresentable items.
Line 3, characters 2-11:
3 |   val x : t
      ^^^^^^^^^
  "x" is not representable.
  The layout of t is any
    because of the definition of t at line 2, characters 2-14.
  But the layout of t must be representable
    because it's the type of an item in a module being projected from.
|}]

let f (module M : S) = M.x
[%%expect{|
Line 1, characters 23-26:
1 | let f (module M : S) = M.x
                           ^^^
Error: This expression projects from a module that contains unrepresentable items.
Line 3, characters 2-11:
3 |   val x : t
      ^^^^^^^^^
  "x" is not representable.
  The layout of t is any
    because of the definition of t at line 2, characters 2-14.
  But the layout of t must be representable
    because it's the type of an item in a module being projected from.
|}]

let f (module M : S) = M.y
[%%expect{|
Line 1, characters 23-26:
1 | let f (module M : S) = M.y
                           ^^^
Error: This expression projects from a module that contains unrepresentable items.
Line 3, characters 2-11:
3 |   val x : t
      ^^^^^^^^^
  "x" is not representable.
  The layout of t is any
    because of the definition of t at line 2, characters 2-14.
  But the layout of t must be representable
    because it's the type of an item in a module being projected from.
|}]

(* We can project out of the module if we know [t]'s layout. *)
let f (type a : value) (module M : S with type t = a) = M.x
[%%expect{|
val f : (module S with type t = 'a) -> 'a = <fun>
|}]

let f (type a : value) (module M : S with type t = a) = M.y
[%%expect{|
val f : (module S with type t = 'a) -> int = <fun>
|}]

(* The same rules apply for projecting out submodules, exceptions, and classes. *)
module type Inner = sig
  val y : int
  val z : int
end

module type S2 = sig
  type t : any
  val x : t
  module N : Inner
  exception Failure
  class foo : unit -> object
      method bar : unit
  end
end
[%%expect{|
module type Inner = sig val y : int val z : int end
module type S2 =
  sig
    type t : any
    val x : t
    module N : Inner
    exception Failure
    class foo : unit -> object method bar : unit end
  end
|}]

let f (module M : S2) = (module M.N : Inner)
[%%expect{|
Line 1, characters 32-35:
1 | let f (module M : S2) = (module M.N : Inner)
                                    ^^^
Error: This expression projects from a module that contains unrepresentable items.
Line 8, characters 2-11:
8 |   val x : t
      ^^^^^^^^^
  "x" is not representable.
  The layout of t is any
    because of the definition of t at line 7, characters 2-14.
  But the layout of t must be representable
    because it's the type of an item in a module being projected from.
|}]

let f (type a : value) (module M : S2 with type t = a) = (module M.N : Inner)
[%%expect{|
val f : (module S2 with type t = 'a) -> (module Inner) = <fun>
|}]

let f (module M : S2) = raise M.Failure
[%%expect{|
Line 1, characters 30-39:
1 | let f (module M : S2) = raise M.Failure
                                  ^^^^^^^^^
Error: This expression projects from a module that contains unrepresentable items.
Line 8, characters 2-11:
8 |   val x : t
      ^^^^^^^^^
  "x" is not representable.
  The layout of t is any
    because of the definition of t at line 7, characters 2-14.
  But the layout of t must be representable
    because it's the type of an item in a module being projected from.
|}]

let f (type a : value) (module M : S2 with type t = a) = raise M.Failure
[%%expect{|
val f : (module S2 with type t = 'a) -> 'b = <fun>
|}]

let f (module M : S2) = new M.foo
[%%expect{|
Line 1, characters 28-33:
1 | let f (module M : S2) = new M.foo
                                ^^^^^
Error: This expression projects from a module that contains unrepresentable items.
Line 8, characters 2-11:
8 |   val x : t
      ^^^^^^^^^
  "x" is not representable.
  The layout of t is any
    because of the definition of t at line 7, characters 2-14.
  But the layout of t must be representable
    because it's the type of an item in a module being projected from.
|}]

let f (type a : value) (module M : S2 with type t = a) = new M.foo
[%%expect{|
val f : (module S2 with type t = 'a) -> unit -> < bar : unit > = <fun>
|}]

(* Same rules apply to submodules of functor arguments. *)
module F (M : S2) = struct module A = M.N end
[%%expect{|
Line 1, characters 38-41:
1 | module F (M : S2) = struct module A = M.N end
                                          ^^^
Error: This expression projects from a module that contains unrepresentable items.
Line 8, characters 2-11:
8 |   val x : t
      ^^^^^^^^^
  "x" is not representable.
  The layout of t is any
    because of the definition of t at line 7, characters 2-14.
  But the layout of t must be representable
    because it's the type of an item in a module being projected from.
|}]

module type Value_witness = sig
  type t : value
end
[%%expect{|
module type Value_witness = sig type t end
|}]

module F (V : Value_witness) (M : S2 with type t = V.t) = struct module A = M.N end
[%%expect{|
module F :
  functor (V : Value_witness)
    (M : sig
           type t = V.t
           val x : t
           module N : Inner
           exception Failure
           class foo : unit -> object method bar : unit end
         end)
    -> sig module A : sig val y : int val z : int end end
|}]

(* Paths that only mention types or module types don't project anything at
   runtime, so they don't require the module to be representable. *)
module type Inner_with_type = sig
  type u
  val z : int
end

module type S3 = sig
  type t : any
  val x : t
  module N : Inner_with_type
end

module F (M : S3) = struct
  type u = M.N.u
  module type T = module type of M.N
end
[%%expect{|
module type Inner_with_type = sig type u val z : int end
module type S3 = sig type t : any val x : t module N : Inner_with_type end
module F :
  functor (M : S3) -> sig type u = M.N.u module type T = Inner_with_type end
|}]

(* Like unrepresentable modules, functor applications have no runtime address.
   Accessing types inside of functors must not force one. *)
module G (X : sig end) = struct
  module N = struct type u = int end
end
module Empty = struct end
type u = G(Empty).N.u
[%%expect{|
module G : functor (X : sig end) -> sig module N : sig type u = int end end
module Empty : sig end
type u = G(Empty).N.u
|}]

(* Can include in signatures without knowing the module representation. *)
module type Including_S = sig
  include S
end
[%%expect{|
module type Including_S = sig type t : any val x : t val y : int end
|}]

(* But we need to know the module representation to include in structures. *)
module F(M : S) = struct
  include M
end
[%%expect{|
Line 2, characters 2-11:
2 |   include M
      ^^^^^^^^^
Error: This module contains unrepresentable items, so it cannot be included.
Line 3, characters 2-11:
3 |   val x : t
      ^^^^^^^^^
  "x" is not representable.
  The layout of t is any
    because of the definition of t at line 2, characters 2-14.
  But the layout of t must be representable
    because it's the type of an item in a module being included.
|}]

module F
    (V : Value_witness)
    (M : S with type t = V.t) = struct
  include M
end
[%%expect{|
module F :
  functor (V : Value_witness)
    (M : sig type t = V.t val x : t val y : int end) ->
    sig type t = V.t val x : t val y : int end
|}]

(* Regression test: templates can close over included values that were originally
   unrepresentable. *)

module F (V : Value_witness) (M : S with type t = V.t) = struct
  include M
  let poly_ g y = (x, y)
end
[%%expect{|
>> Fatal error: unrepresentable signature item
Uncaught exception: Misc.Fatal_error

|}]

module F (V : Value_witness) (M : S with type t = V.t) = struct
  include M
  module G (X : sig end @ static) = struct let y = x end
end
[%%expect{|
>> Fatal error: unrepresentable signature item
Uncaught exception: Misc.Fatal_error

|}]

(* Same for [open]ed values. *)

module F (V : Value_witness) (M : S with type t = V.t) = struct
  open M
  let poly_ g y = (x, y)
end
[%%expect{|
module F :
  functor (V : Value_witness)
    (M : sig type t = V.t val x : t val y : int end) ->
    sig val poly_ g : 'a -> M.t * 'a end
|}]

module F (V : Value_witness) (M : S with type t = V.t) = struct
  open M
  module G (X : sig end @ static) = struct let y = x end
end
[%%expect{|
module F :
  functor (V : Value_witness)
    (M : sig type t = V.t val x : t val y : int end) ->
    sig module G : functor (X : sig end @ static) -> sig val y : M.t end end
|}]

(* [open]ing a module expression binds its items like an [include] does *)
module F (V : Value_witness) (M : S with type t = V.t) = struct
  open (M : S with type t = V.t)
  let poly_ g y = (x, y)
end
[%%expect {|
>> Fatal error: unrepresentable signature item
Uncaught exception: Misc.Fatal_error

|}]

module F (V : Value_witness) (M : S with type t = V.t) = struct
  open (M : S with type t = V.t)
  module G (X : sig end @ static) = struct let y = x end
end
[%%expect {|
>> Fatal error: unrepresentable signature item
Uncaught exception: Misc.Fatal_error

|}]

(* In general, we can't open a module without knowing its representation. *)

module F (M : S) = struct
  (* This is permitted because its addresses gets resolved lazily *)
  open M
end
[%%expect {|
module F : functor (M : S) -> sig end
|}]

module F (M : S) = struct
  open (M : S)
end
[%%expect {|
>> Fatal error: unrepresentable signature item
Uncaught exception: Misc.Fatal_error

|}]

module F (V : Value_witness) (M : S with type t = V.t) = struct
  open (M : S with type t = V.t)
end
[%%expect {|
>> Fatal error: unrepresentable signature item
Uncaught exception: Misc.Fatal_error

|}]

let f (m : (module S)) =
  let open (val m) in
  ()
[%%expect {|
>> Fatal error: unrepresentable signature item
Uncaught exception: Misc.Fatal_error

|}]

let f (type a : value) (m : (module S with type t = a)) =
  let open (val m) in
  ()
[%%expect {|
>> Fatal error: unrepresentable signature item
Uncaught exception: Misc.Fatal_error

|}]

module G (X : sig end) : S = struct
  type t = int
  let x = 1
  let y = 2
end
[%%expect {|
module G : functor (X : sig end) -> S
|}]

module F (X : sig end) = struct
  open G(X)
end
[%%expect {|
>> Fatal error: unrepresentable signature item
Uncaught exception: Misc.Fatal_error

|}]

(* Opens in signatures only take paths and bind nothing at runtime, so they
   don't require the module to be representable. *)
module type U = sig
  module M : S
  open M
  val z : t
end
[%%expect {|
module type U = sig module M : S val z : M.t end
|}]

(* Signature inclusion checks with t at different layouts *)
module Sig_ok : S = struct
  type t = int
  let x = 1
  let y = 2
end
[%%expect{|
module Sig_ok : S
|}]

module Sig_ok2 : S = struct
  type t = float#
  let x = #42.5
  let y = 2
end
[%%expect{|
module Sig_ok2 : S
|}]

(* Signature inclusion with nontrivial coercions *)
module Sig_extra : S = struct
  type t = int
  let x = 1
  let y = 2
  let z = 3 (* This gets dropped *)
end
[%%expect{|
module Sig_extra : S
|}]

module Sig_reorder : S = struct
  type t = int
  let y = 2 (* These get reordered *)
  let x = 1
end
[%%expect{|
module Sig_reorder : S
|}]

(* Signature inclusion *)

module type U = sig
  val y : int
end

module G (X : U) = struct type u = int end
[%%expect{|
module type U = sig val y : int end
module G : functor (X : U) -> sig type u = int end
|}]

(* CR-soon jkerrigan: The only reason this is rejected is because we check
   signature inclusion by building a coercion. Fix this by introducing a
   separate check that doesn't build one. *)
module F (M : S) = struct
  (* [G(M).u] checks [S <: U] at the type level. *)
  type v = G(M).u
end
[%%expect{|
Line 3, characters 11-15:
3 |   type v = G(M).u
               ^^^^
Error: Modules do not match: sig type t = M.t val x : t val y : int end
     is not included in U
     Signatures containing unrepresentable items are not
     included in other signatures.
     "x" is not representable.
     The layout of t is any
       because of the definition of t at line 2, characters 2-14.
     But the layout of t must be representable
       because it's the type of an item in a module being coerced.
|}]

module F (V : Value_witness) (M : S with type t = V.t) = struct
  type v = G(M).u
end
[%%expect{|
module F :
  functor (V : Value_witness)
    (M : sig type t = V.t val x : t val y : int end) ->
    sig type v = G(M).u end
|}]

(* Coercing out of a module requires knowing the input's representation. *)
module Sig_from_unrepresentable (M : S) = struct
  module N : sig val y : int end = M
end
[%%expect{|
Line 2, characters 35-36:
2 |   module N : sig val y : int end = M
                                       ^
Error: Signature mismatch:
       Modules do not match:
         sig type t = M.t val x : t val y : int end
       is not included in
         sig val y : int end
       Signatures containing unrepresentable items are not
       included in other signatures.
       "x" is not representable.
       The layout of t is any
         because of the definition of t at line 2, characters 2-14.
       But the layout of t must be representable
         because it's the type of an item in a module being coerced.
|}]

(* The same, in a submodule. *)
module Sig_from_unrepresentable_nested (M : S) = struct
  module O : sig module N : sig val y : int end end = struct module N = M end
end
[%%expect{|
Line 2, characters 54-77:
2 |   module O : sig module N : sig val y : int end end = struct module N = M end
                                                          ^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig module N : sig type t = M.t val x : t val y : int end end
       is not included in
         sig module N : sig val y : int end end
       In module "N":
       Modules do not match:
         sig type t = M.t val x : t val y : int end
       is not included in
         sig val y : int end
       In module "N":
       Signatures containing unrepresentable items are not
       included in other signatures.
       "x" is not representable.
       The layout of t is any
         because of the definition of t at line 2, characters 2-14.
       But the layout of t must be representable
         because it's the type of an item in a module being coerced.
|}]

(* We can obtain sorts for things that don't have a runtime representation in
   the structure but do in the signature. *)
module Sig_primitive : sig type t : any val f : t end = struct
  type t = int -> int
  external f : int -> int = "%identity"
end
[%%expect{|
module Sig_primitive : sig type t : any val f : t end
|}]

(* The constant and identity functors can take unrepresentable modules. *)
module F (M : S) = struct end
[%%expect{|
module F : functor (M : S) -> sig end
|}]

module F (M : S) = M
[%%expect{|
module F : functor (M : S) -> sig type t = M.t val x : t val y : int end
|}]

(* We're even able to take M's module type. *)
module F (M : S) = struct
  module type S1 = module type of M
end
[%%expect{|
module F : functor (M : S) -> sig module type S1 = S end
|}]

(* Unpacking and repacking an unrepresentable module is allowed, since no
   conversion is needed. *)
let f (module M : S) = (module M : S)
[%%expect{|
val f : (module S) -> (module S) = <fun>
|}]

module type S2 = sig
  type t : any
  val x : t
end

(* We need to know representations to unpack and repack at different types. *)
let f (module M : S) = (module M : S2)
[%%expect{|
module type S2 = sig type t : any val x : t end
Line 7, characters 31-32:
7 | let f (module M : S) = (module M : S2)
                                   ^
Error: Signature mismatch:
       Modules do not match:
         sig type t = M.t val x : t val y : int end
       is not included in
         S2
       Signatures containing unrepresentable items are not
       included in other signatures.
       "x" is not representable.
       The layout of t is any
         because of the definition of t at line 2, characters 2-14.
       But the layout of t must be representable
         because it's the type of an item in a module being coerced.
|}]

let f (type a : value) (module M : S with type t = a) = (module M : S2)
[%%expect{|
val f : (module S with type t = 'a) -> (module S2) = <fun>
|}]

(* Test [include functor]. *)
module type S = sig
  type t : any
  val x : t
end
[%%expect{|
module type S = sig type t : any val x : t end
|}]

module F(M : S) = struct
  let y = 42
end
[%%expect{|
module F : functor (M : S) -> sig val y : int end
|}]

module M1 = struct
  type t = int
  let x = 42
  include functor F
end
[%%expect{|
>> Fatal error: unrepresentable signature item
Uncaught exception: Misc.Fatal_error

|}]

(* Same signature can be used at two different layouts *)
module M2 = struct
  type t = float#
  let x = #42.0
  include functor F
end
[%%expect{|
>> Fatal error: unrepresentable signature item
Uncaught exception: Misc.Fatal_error

|}]

(* Generative functor *)
module F_gen(M : S)() = struct
  let y = 42
end

module M3 = struct
  type t = int
  let x = 42
  include functor F_gen
end
[%%expect{|
module F_gen : functor (M : S) () -> sig val y : int end
>> Fatal error: unrepresentable signature item
Uncaught exception: Misc.Fatal_error

|}]

(* Includes in signatures have no runtime argument, so the items passed to the
   functor don't need to be representable. *)
module type F_type = functor (M : S) -> sig val y : int end
module type T = sig
  type t : any
  val x : t
  include functor F_type
end
[%%expect{|
module type F_type = functor (M : S) -> sig val y : int end
>> Fatal error: unrepresentable signature item
Uncaught exception: Misc.Fatal_error

|}]

(* Module aliases. A bare alias to a path inside an unrepresentable module is
   fine, because nothing is projected at runtime. *)
module type S2 = sig
  type t : any
  val x : t
  module N : Inner
end

module P : S2 = struct
  type t = int
  let x = 1
  module N = struct let y = 1 let z = 2 end
end

module A = P.N
[%%expect{|
module type S2 = sig type t : any val x : t module N : Inner end
module P : S2
module A = P.N
|}]

(* Constraining an alias makes the module exist at runtime, which requires
   projecting [N] out of [P]. *)
module A : Inner = P.N
[%%expect{|
Uncaught exception: Env.Unrepresentable_type(_, _, _, _)

|}]

module M : sig module A : Inner end = struct module A = P.N end
[%%expect{|
Uncaught exception: Env.Unrepresentable_type(_, _, _, _)

|}]
