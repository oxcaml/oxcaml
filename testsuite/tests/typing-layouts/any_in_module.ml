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
Line 3, characters 10-11:
3 |   val x : t
              ^
Error: The type of a module-level value must have a representable layout.
       The layout of type t is any
         because of the definition of t at line 2, characters 2-14.
       But the layout of type t must be representable
         because it's the type of something in a signature.
|}]

(* We can't project out of a module without knowing its representation. *)
module M : S = struct
  type t = int
  let x = 42
  let y = 43
end

let x = M.x
[%%expect{|
Line 1, characters 11-12:
1 | module M : S = struct
               ^
Error: Unbound module type "S"
|}]

let y = M.y
[%%expect{|
Line 1, characters 8-9:
1 | let y = M.y
            ^
Error: Unbound module "M"
|}]

let f (module M : S) = M.x
[%%expect{|
Line 1, characters 18-19:
1 | let f (module M : S) = M.x
                      ^
Error: Unbound module type "S"
|}]

let f (module M : S) = M.y
[%%expect{|
Line 1, characters 18-19:
1 | let f (module M : S) = M.y
                      ^
Error: Unbound module type "S"
|}]

(* We can project out of the module if we know [t]'s layout. *)
let f (type a : value) (module M : S with type t = a) = M.x
[%%expect{|
Line 1, characters 35-52:
1 | let f (type a : value) (module M : S with type t = a) = M.x
                                       ^^^^^^^^^^^^^^^^^
Error: Unbound module type "S"
|}]

let f (type a : value) (module M : S with type t = a) = M.y
[%%expect{|
Line 1, characters 35-52:
1 | let f (type a : value) (module M : S with type t = a) = M.y
                                       ^^^^^^^^^^^^^^^^^
Error: Unbound module type "S"
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
Line 8, characters 10-11:
8 |   val x : t
              ^
Error: The type of a module-level value must have a representable layout.
       The layout of type t is any
         because of the definition of t at line 7, characters 2-14.
       But the layout of type t must be representable
         because it's the type of something in a signature.
|}]

let f (module M : S2) = (module M.N : Inner)
[%%expect{|
Line 1, characters 18-20:
1 | let f (module M : S2) = (module M.N : Inner)
                      ^^
Error: Unbound module type "S2"
|}]

let f (type a : value) (module M : S2 with type t = a) = (module M.N : Inner)
[%%expect{|
Line 1, characters 35-53:
1 | let f (type a : value) (module M : S2 with type t = a) = (module M.N : Inner)
                                       ^^^^^^^^^^^^^^^^^^
Error: Unbound module type "S2"
|}]

let f (module M : S2) = raise M.Failure
[%%expect{|
Line 1, characters 18-20:
1 | let f (module M : S2) = raise M.Failure
                      ^^
Error: Unbound module type "S2"
|}]

let f (type a : value) (module M : S2 with type t = a) = raise M.Failure
[%%expect{|
Line 1, characters 35-53:
1 | let f (type a : value) (module M : S2 with type t = a) = raise M.Failure
                                       ^^^^^^^^^^^^^^^^^^
Error: Unbound module type "S2"
|}]

let f (module M : S2) = new M.foo
[%%expect{|
Line 1, characters 18-20:
1 | let f (module M : S2) = new M.foo
                      ^^
Error: Unbound module type "S2"
|}]

let f (type a : value) (module M : S2 with type t = a) = new M.foo
[%%expect{|
Line 1, characters 35-53:
1 | let f (type a : value) (module M : S2 with type t = a) = new M.foo
                                       ^^^^^^^^^^^^^^^^^^
Error: Unbound module type "S2"
|}]

(* Same rules apply to submodules of functor arguments. *)
module F (M : S2) = struct module A = M.N end
[%%expect{|
Line 1, characters 14-16:
1 | module F (M : S2) = struct module A = M.N end
                  ^^
Error: Unbound module type "S2"
|}]

module type Value_witness = sig
  type t : value
end
[%%expect{|
module type Value_witness = sig type t end
|}]

module F (V : Value_witness) (M : S2 with type t = V.t) = struct module A = M.N end
[%%expect{|
Line 1, characters 34-36:
1 | module F (V : Value_witness) (M : S2 with type t = V.t) = struct module A = M.N end
                                      ^^
Error: Unbound module type "S2"
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
Line 8, characters 10-11:
8 |   val x : t
              ^
Error: The type of a module-level value must have a representable layout.
       The layout of type t is any
         because of the definition of t at line 7, characters 2-14.
       But the layout of type t must be representable
         because it's the type of something in a signature.
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
Line 2, characters 10-11:
2 |   include S
              ^
Error: Unbound module type "S"
|}]

(* But we need to know the module representation to include in structures. *)
module F(M : S) = struct
  include M
end
[%%expect{|
Line 1, characters 13-14:
1 | module F(M : S) = struct
                 ^
Error: Unbound module type "S"
|}]

module F
    (V : Value_witness)
    (M : S with type t = V.t) = struct
  include M
end
[%%expect{|
Line 3, characters 9-10:
3 |     (M : S with type t = V.t) = struct
             ^
Error: Unbound module type "S"
|}]

(* Regression test: templates can close over included values that were originally
   unrepresentable. *)

module F (V : Value_witness) (M : S with type t = V.t) = struct
  include M
  let poly_ g y = (x, y)
end
[%%expect{|
Line 1, characters 34-35:
1 | module F (V : Value_witness) (M : S with type t = V.t) = struct
                                      ^
Error: Unbound module type "S"
|}]

module F (V : Value_witness) (M : S with type t = V.t) = struct
  include M
  module G (X : sig end @ static) = struct let y = x end
end
[%%expect{|
Line 1, characters 34-35:
1 | module F (V : Value_witness) (M : S with type t = V.t) = struct
                                      ^
Error: Unbound module type "S"
|}]

(* Same for [open]ed values. *)

module F (V : Value_witness) (M : S with type t = V.t) = struct
  open M
  let poly_ g y = (x, y)
end
[%%expect{|
Line 1, characters 34-35:
1 | module F (V : Value_witness) (M : S with type t = V.t) = struct
                                      ^
Error: Unbound module type "S"
|}]

module F (V : Value_witness) (M : S with type t = V.t) = struct
  open M
  module G (X : sig end @ static) = struct let y = x end
end
[%%expect{|
Line 1, characters 34-35:
1 | module F (V : Value_witness) (M : S with type t = V.t) = struct
                                      ^
Error: Unbound module type "S"
|}]

(* [open]ing a module expression binds its items like an [include] does *)
module F (V : Value_witness) (M : S with type t = V.t) = struct
  open (M : S with type t = V.t)
  let poly_ g y = (x, y)
end
[%%expect {|
Line 1, characters 34-35:
1 | module F (V : Value_witness) (M : S with type t = V.t) = struct
                                      ^
Error: Unbound module type "S"
|}]

module F (V : Value_witness) (M : S with type t = V.t) = struct
  open (M : S with type t = V.t)
  module G (X : sig end @ static) = struct let y = x end
end
[%%expect {|
Line 1, characters 34-35:
1 | module F (V : Value_witness) (M : S with type t = V.t) = struct
                                      ^
Error: Unbound module type "S"
|}]

(* In general, we can't open a module without knowing its representation. *)

module F (M : S) = struct
  (* This is permitted because its addresses gets resolved lazily *)
  open M
end
[%%expect {|
Line 1, characters 14-15:
1 | module F (M : S) = struct
                  ^
Error: Unbound module type "S"
|}]

module F (M : S) = struct
  open (M : S)
end
[%%expect {|
Line 1, characters 14-15:
1 | module F (M : S) = struct
                  ^
Error: Unbound module type "S"
|}]

module F (V : Value_witness) (M : S with type t = V.t) = struct
  open (M : S with type t = V.t)
end
[%%expect {|
Line 1, characters 34-35:
1 | module F (V : Value_witness) (M : S with type t = V.t) = struct
                                      ^
Error: Unbound module type "S"
|}]

let f (m : (module S)) =
  let open (val m) in
  ()
[%%expect {|
Line 1, characters 19-20:
1 | let f (m : (module S)) =
                       ^
Error: Unbound module type "S"
|}]

let f (type a : value) (m : (module S with type t = a)) =
  let open (val m) in
  ()
[%%expect {|
Line 1, characters 36-53:
1 | let f (type a : value) (m : (module S with type t = a)) =
                                        ^^^^^^^^^^^^^^^^^
Error: Unbound module type "S"
|}]

module G (X : sig end) : S = struct
  type t = int
  let x = 1
  let y = 2
end
[%%expect {|
Line 1, characters 25-26:
1 | module G (X : sig end) : S = struct
                             ^
Error: Unbound module type "S"
|}]

module F (X : sig end) = struct
  open G(X)
end
[%%expect {|
module F : functor (X : sig end) -> sig end
|}]

(* Opens in signatures only take paths and bind nothing at runtime, so they
   don't require the module to be representable. *)
module type U = sig
  module M : S
  open M
  val z : t
end
[%%expect {|
Line 2, characters 13-14:
2 |   module M : S
                 ^
Error: Unbound module type "S"
|}]

(* Signature inclusion checks with t at different layouts *)
module Sig_ok : S = struct
  type t = int
  let x = 1
  let y = 2
end
[%%expect{|
Line 1, characters 16-17:
1 | module Sig_ok : S = struct
                    ^
Error: Unbound module type "S"
|}]

module Sig_ok2 : S = struct
  type t = float#
  let x = #42.5
  let y = 2
end
[%%expect{|
Line 1, characters 17-18:
1 | module Sig_ok2 : S = struct
                     ^
Error: Unbound module type "S"
|}]

(* Signature inclusion with nontrivial coercions *)
module Sig_extra : S = struct
  type t = int
  let x = 1
  let y = 2
  let z = 3 (* This gets dropped *)
end
[%%expect{|
Line 1, characters 19-20:
1 | module Sig_extra : S = struct
                       ^
Error: Unbound module type "S"
|}]

module Sig_reorder : S = struct
  type t = int
  let y = 2 (* These get reordered *)
  let x = 1
end
[%%expect{|
Line 1, characters 21-22:
1 | module Sig_reorder : S = struct
                         ^
Error: Unbound module type "S"
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
Line 1, characters 14-15:
1 | module F (M : S) = struct
                  ^
Error: Unbound module type "S"
|}]

module F (V : Value_witness) (M : S with type t = V.t) = struct
  type v = G(M).u
end
[%%expect{|
Line 1, characters 34-35:
1 | module F (V : Value_witness) (M : S with type t = V.t) = struct
                                      ^
Error: Unbound module type "S"
|}]

(* Coercing out of a module requires knowing the input's representation. *)
module Sig_from_unrepresentable (M : S) = struct
  module N : sig val y : int end = M
end
[%%expect{|
Line 1, characters 37-38:
1 | module Sig_from_unrepresentable (M : S) = struct
                                         ^
Error: Unbound module type "S"
|}]

(* The same, in a submodule. *)
module Sig_from_unrepresentable_nested (M : S) = struct
  module O : sig module N : sig val y : int end end = struct module N = M end
end
[%%expect{|
Line 1, characters 44-45:
1 | module Sig_from_unrepresentable_nested (M : S) = struct
                                                ^
Error: Unbound module type "S"
|}]

(* We can obtain sorts for things that don't have a runtime representation in
   the structure but do in the signature. *)
module Sig_primitive : sig type t : any val f : t end = struct
  type t = int -> int
  external f : int -> int = "%identity"
end
[%%expect{|
Line 1, characters 48-49:
1 | module Sig_primitive : sig type t : any val f : t end = struct
                                                    ^
Error: The type of a module-level value must have a representable layout.
       The layout of type t is any
         because of the definition of t at line 1, characters 27-39.
       But the layout of type t must be representable
         because it's the type of something in a signature.
|}]

(* The constant and identity functors can take unrepresentable modules. *)
module F (M : S) = struct end
[%%expect{|
Line 1, characters 14-15:
1 | module F (M : S) = struct end
                  ^
Error: Unbound module type "S"
|}]

module F (M : S) = M
[%%expect{|
Line 1, characters 14-15:
1 | module F (M : S) = M
                  ^
Error: Unbound module type "S"
|}]

(* We're even able to take M's module type. *)
module F (M : S) = struct
  module type S1 = module type of M
end
[%%expect{|
Line 1, characters 14-15:
1 | module F (M : S) = struct
                  ^
Error: Unbound module type "S"
|}]

(* Unpacking and repacking an unrepresentable module is allowed, since no
   conversion is needed. *)
let f (module M : S) = (module M : S)
[%%expect{|
Line 1, characters 18-19:
1 | let f (module M : S) = (module M : S)
                      ^
Error: Unbound module type "S"
|}]

module type S2 = sig
  type t : any
  val x : t
end

(* We need to know representations to unpack and repack at different types. *)
let f (module M : S) = (module M : S2)
[%%expect{|
Line 3, characters 10-11:
3 |   val x : t
              ^
Error: The type of a module-level value must have a representable layout.
       The layout of type t/2 is any
         because of the definition of t at line 2, characters 2-14.
       But the layout of type t/2 must be representable
         because it's the type of something in a signature.
|}]

let f (type a : value) (module M : S with type t = a) = (module M : S2)
[%%expect{|
Line 1, characters 35-52:
1 | let f (type a : value) (module M : S with type t = a) = (module M : S2)
                                       ^^^^^^^^^^^^^^^^^
Error: Unbound module type "S"
|}]

(* Test [include functor]. *)
module type S = sig
  type t : any
  val x : t
end
[%%expect{|
Line 3, characters 10-11:
3 |   val x : t
              ^
Error: The type of a module-level value must have a representable layout.
       The layout of type t/3 is any
         because of the definition of t at line 2, characters 2-14.
       But the layout of type t/3 must be representable
         because it's the type of something in a signature.
|}]

module F(M : S) = struct
  let y = 42
end
[%%expect{|
Line 1, characters 13-14:
1 | module F(M : S) = struct
                 ^
Error: Unbound module type "S"
|}]

module M1 = struct
  type t = int
  let x = 42
  include functor F
end
[%%expect{|
module M1 : sig type t = int val x : int end
|}]

(* Same signature can be used at two different layouts *)
module M2 = struct
  type t = float#
  let x = #42.0
  include functor F
end
[%%expect{|
module M2 : sig type t = float# val x : float# end
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
Line 1, characters 17-18:
1 | module F_gen(M : S)() = struct
                     ^
Error: Unbound module type "S"
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
Line 1, characters 34-35:
1 | module type F_type = functor (M : S) -> sig val y : int end
                                      ^
Error: Unbound module type "S"
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
Line 3, characters 10-11:
3 |   val x : t
              ^
Error: The type of a module-level value must have a representable layout.
       The layout of type t is any
         because of the definition of t at line 2, characters 2-14.
       But the layout of type t must be representable
         because it's the type of something in a signature.
|}]

(* Constraining an alias makes the module exist at runtime, which requires
   projecting [N] out of [P]. *)
module A : Inner = P.N
[%%expect{|
Line 1, characters 19-20:
1 | module A : Inner = P.N
                       ^
Error: Unbound module "P"
|}]

module M : sig module A : Inner end = struct module A = P.N end
[%%expect{|
Line 1, characters 56-57:
1 | module M : sig module A : Inner end = struct module A = P.N end
                                                            ^
Error: Unbound module "P"
|}]
