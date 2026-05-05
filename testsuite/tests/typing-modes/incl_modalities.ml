(* TEST
    flags += "-extension mode_alpha";
    expect;
*)

(* Including module type with modalities *)
module type S = sig
  val foo : 'a -> 'a

  val bar : 'a -> 'a @@ nonportable

  val baz : 'a -> 'a @@ portable
end
[%%expect{|
module type S =
  sig
    val foo : 'a -> 'a
    val bar : 'a -> 'a
    val baz : 'a -> 'a @@ portable
  end
|}]

module type S' = sig
  include S @@ portable
end
[%%expect{|
module type S' =
  sig
    val foo : 'a -> 'a @@ portable
    val bar : 'a -> 'a @@ portable
    val baz : 'a -> 'a @@ portable
  end
|}]

module type S' = sig
  include S @@ nonportable
end
[%%expect{|
module type S' =
  sig
    val foo : 'a -> 'a
    val bar : 'a -> 'a
    val baz : 'a -> 'a @@ portable
  end
|}]

(* Include functor module types with modalities *)
module type S = functor (_ : sig end) -> sig
  val foo : 'a -> 'a

  val bar : 'a -> 'a @@ nonportable

  val baz : 'a -> 'a @@ portable
end
[%%expect{|
module type S =
  sig end ->
    sig
      val foo : 'a -> 'a
      val bar : 'a -> 'a
      val baz : 'a -> 'a @@ portable
    end
|}]

module type S' = sig
  include functor S @@ portable
end
[%%expect{|
module type S' =
  sig
    val foo : 'a -> 'a @@ portable
    val bar : 'a -> 'a @@ portable
    val baz : 'a -> 'a @@ portable
  end
|}]

module type S' = sig
  include functor S @@ nonportable
end
[%%expect{|
module type S' =
  sig
    val foo : 'a -> 'a
    val bar : 'a -> 'a
    val baz : 'a -> 'a @@ portable
  end
|}]

(* CR zqian: add tests of recursive modules & include w/ modalties, once
   modules can have modes. *)

module type S = sig
  val bar : 'a -> 'a
  module M : sig
    val foo : 'a -> 'a
  end
end
[%%expect{|
module type S =
  sig val bar : 'a -> 'a module M : sig val foo : 'a -> 'a end end
|}]

module type S' = sig
  include S @@ portable
end
[%%expect{|
module type S' =
  sig
    val bar : 'a -> 'a @@ portable
    module M : sig val foo : 'a -> 'a end @@ portable
  end
|}]

module type S' = sig
  include [@no_recursive_modalities] S @@ portable
end
[%%expect{|
module type S' =
  sig
    val bar : 'a -> 'a @@ portable
    module M : sig val foo : 'a -> 'a end
  end
|}]

module type T = sig
  val baz : 'a -> 'a
  module M : S
end
[%%expect{|
module type T = sig val baz : 'a -> 'a module M : S end
|}]

module type T' = sig
  include T @@ portable
end
[%%expect{|
module type T' =
  sig val baz : 'a -> 'a @@ portable module M : S @@ portable end
|}]

module type T' = sig
  include [@no_recursive_modalities] T @@ portable
end
[%%expect{|
module type T' = sig val baz : 'a -> 'a @@ portable module M : S end
|}]

(* submodule whose type is in the signature *)
module type S = sig
  module type MT = sig
    val foo : 'a -> 'a
  end
  module M : MT
end

module type S' = sig
  include S @@ portable
end
[%%expect{|
module type S =
  sig module type MT = sig val foo : 'a -> 'a end module M : MT end
module type S' =
  sig
    module type MT = sig val foo : 'a -> 'a end
    module M : MT @@ portable
  end
|}]

(* and this works deeply *)
module type S = sig
  module type MT = sig
    val foo : 'a -> 'a
  end
  module M : sig
    module N : MT
  end
end
module type S' = sig
    include S @@ portable
end
[%%expect{|
module type S =
  sig
    module type MT = sig val foo : 'a -> 'a end
    module M : sig module N : MT end
  end
module type S' =
  sig
    module type MT = sig val foo : 'a -> 'a end
    module M : sig module N : MT end @@ portable
  end
|}]

(* submodule whose type is not in the signature but inside a module *)
module M = struct
  module type Foo = sig
    val foo : 'a -> 'a
  end
  module type Foo' = Foo
  module type S = sig
    module N : Foo'
  end
end
module type S' = sig
  include M.S @@ portable
end
[%%expect{|
module M :
  sig
    module type Foo = sig val foo : 'a -> 'a end
    module type Foo' = Foo
    module type S = sig module N : Foo' end
  end @@ stateless
module type S' = sig module N : M.Foo' @@ portable end
|}]

(* include abstract module type is still not allowed *)
module type S = sig
  module type MT
  include MT @@ portable
end
[%%expect{|
Line 3, characters 10-12:
3 |   include MT @@ portable
              ^^
Error: This module type is not a signature
|}]

(* submodule of abstract type is not affected by modality *)
module type MT
module type S = sig
  module M : MT
end
module type S' = sig
  include S @@ portable
end
[%%expect{|
module type MT
module type S = sig module M : MT end
module type S' = sig module M : MT @@ portable end
|}]

(* strenghtened module type *)
module type S = sig
  module type T = sig
    type a
    val baz : a
    val foo : a -> a
  end
  module MT : T
  module M : T with MT
end
module type S' = sig
  include S @@ portable
end
[%%expect{|
module type S =
  sig
    module type T = sig type a val baz : a val foo : a -> a end
    module MT : T
    module M : sig type a = MT.a val baz : a val foo : a -> a end
  end
module type S' =
  sig
    module type T = sig type a val baz : a val foo : a -> a end
    module MT : T @@ portable
    module M : sig type a = MT.a val baz : a val foo : a -> a end @@ portable
  end
|}]

(* default modalities affect include modalities, which is deep. *)
module type T = sig
  module M : sig
    val foo : 'a -> 'a
  end
end
[%%expect{|
module type T = sig module M : sig val foo : 'a -> 'a end end
|}]

module type T = sig @@ portable
  include T
end
[%%expect{|
module type T = sig module M : sig val foo : 'a -> 'a end @@ portable end
|}]

(* signature with default modality, include with a different axis *)
module type S = sig @@ portable
  val foo : 'a -> 'a
end

module type S' = sig
  include S @@ contended
end
[%%expect{|
module type S = sig val foo : 'a -> 'a @@ portable end
module type S' = sig val foo : 'a -> 'a @@ portable contended end
|}]

(* signature with default modality, include does not override it *)
module type S = sig @@ portable
  val foo : 'a -> 'a
end

module type S' = sig
  include S @@ nonportable contended
end
[%%expect{|
module type S = sig val foo : 'a -> 'a @@ portable end
module type S' = sig val foo : 'a -> 'a @@ portable contended end
|}]

(* signature S' with default modality, include adds modality on different axis *)
module type S = sig
  val foo : 'a -> 'a
end

module type S' = sig @@ portable
  include S @@ contended
end
[%%expect{|
module type S = sig val foo : 'a -> 'a end
module type S' = sig val foo : 'a -> 'a @@ portable contended end
|}]

(* signature S' with default modality, include overrides it *)
module type S' = sig @@ portable
  include S @@ nonportable contended
end
[%%expect{|
module type S' = sig val foo : 'a -> 'a @@ contended end
|}]

(* include a nonportable module in a portable functor is allowed if the
   module is empty - no items are imported so no lock needs to be walked *)
module (Foo @ nonportable) = struct end

module (Bar @ portable) (M : sig end) = struct
  include Foo
end
[%%expect{|
module Foo : sig end @@ stateless nonportable
module Bar : functor (M : sig end) -> sig end @@ stateless
|}]

(* if Foo contains a stateless identity function, it still passes *)
module (Foo @ nonportable) = struct
  let foo x = x
end

module (Bar @ portable) (M : sig end) = struct
  include Foo
end
[%%expect{|
module Foo : sig val foo : 'a -> 'a @@ portable end @@ stateless nonportable
module Bar : functor (M : sig end) -> sig val foo : 'a -> 'a @@ stateless end
  @@ stateless
|}]

(* if Foo contains a function closing over a mutable ref, it fails *)
let x = ref 0

module (Foo @ nonportable) = struct
  let bar () = x := 24
end

module (Bar @ portable) (M : sig end) = struct
  include Foo
end
[%%expect{|
val x : int ref = {contents = 0}
module Foo : sig val bar : unit -> unit end
Lines 7-9, characters 24-3:
7 | ........................(M : sig end) = struct
8 |   include Foo
9 | end
Error: The module is "nonportable"
         because it closes over the value "Foo.bar" at line 8, characters 10-13
         which is "nonportable"
         because it contains a usage (of the value "x" at line 4, characters 15-16)
         which is expected to be "uncontended".
       However, the module highlighted is expected to be "portable".
|}]

(* if Foo contains a string (naturally stateless), it passes *)
module (Foo @ nonportable) = struct
  let s = "hello"
end

module (Bar @ portable) (M : sig end) = struct
  include Foo
end
[%%expect{|
module Foo : sig val s : string @@ portable end @@ stateless nonportable
module Bar : functor (M : sig end) -> sig val s : string @@ stateless end @@
  stateless
|}]

(* if Foo contains a mutable ref, in Bar after inclusion x is contended *)
module (Foo @ nonportable) = struct
  let x = ref 0
end

module (Bar @ portable) (M : sig end) = struct
  include Foo
  let _ = (x : int ref @ contended)
end
[%%expect{|
module Foo : sig val x : int ref end
module Bar :
  functor (M : sig end) -> sig val x : int ref @@ stateless immutable end @@
  stateless
|}]

(* using x as uncontended in Bar fails *)
module (Bar @ portable) (M : sig end) = struct
  include Foo
  let _ = (x : int ref @ uncontended)
end
[%%expect{|
Lines 1-4, characters 24-3:
1 | ........................(M : sig end) = struct
2 |   include Foo
3 |   let _ = (x : int ref @ uncontended)
4 | end
Error: The module is "nonportable"
         because it contains a usage (of the value "Foo.x" at line 2, characters 10-13)
         which is expected to be "uncontended".
       However, the module highlighted is expected to be "portable".
|}]

(* if Foo is local and empty, including it in a global Bar fails even though
   there are no items - the memaddr check catches the locality violation *)
let f () =
  let module Foo @ local = struct end in
  let module Bar (M : sig end) = struct
    include Foo
  end in
  ignore Bar
[%%expect{|
Line 4, characters 12-15:
4 |     include Foo
                ^^^
Error: The module "Foo" is "local"
       but is expected to be "global"
         because it is used inside the functor at lines 3-5, characters 17-5
         which is expected to be "global"
         because modules always need to be allocated on the heap.
|}]
