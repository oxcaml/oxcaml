(* TEST
 expect;
*)

(* Implicit kinds in signatures. *)

module type S0 = sig
  [@@@implicit_kind: ('elt : bits64)]

  val f : 'elt -> 'elt array
end

[%%expect{|
module type S0 = sig val f : ('elt : bits64). 'elt -> 'elt array end
|}]

(* CR implicit-variables: implicit kinds don't work in structures. *)

module M = struct
  [@@@implicit_kind: ('elt : bits64)]

  let f : 'elt -> 'elt array = fun x -> [| x |]
end

[%%expect{|
module M : sig val f : 'elt -> 'elt array end
|}]

(* Implicit kind without jkind annotation fails. *)

module type S1 = sig
  [@@@implicit_kind: 'a]
end

[%%expect{|
Line 2, characters 2-24:
2 |   [@@@implicit_kind: 'a]
      ^^^^^^^^^^^^^^^^^^^^^^
Warning 47 [attribute-payload]: illegal payload for attribute 'implicit_kind'.
implicit_kind attribute expects: ('var1 : jkind1) * ('var2 : jkind2) ...

module type S1 = sig end
|}]

(* Invalid syntax - not a type variable: *)

module type S2 = sig
  [@@@implicit_kind: int]
end


[%%expect{|
Line 2, characters 2-25:
2 |   [@@@implicit_kind: int]
      ^^^^^^^^^^^^^^^^^^^^^^^
Warning 47 [attribute-payload]: illegal payload for attribute 'implicit_kind'.
implicit_kind attribute expects: ('var1 : jkind1) * ('var2 : jkind2) ...

module type S2 = sig end
|}]

(* Underscore is not supported for jkinds. *)

module type S3 = sig
  [@@@implicit_kind: ('a : _)]

  val x : 'a -> 'a
end

[%%expect{|
Line 2, characters 27-28:
2 |   [@@@implicit_kind: ('a : _)]
                               ^
Error: Unimplemented kind syntax
|}]

(* Wildcards are not supported. *)
module type S4 = sig
  [@@@implicit_kind: (_ : bits32)]

  val a : _ -> _
end
[%%expect{|
Line 2, characters 2-34:
2 |   [@@@implicit_kind: (_ : bits32)]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 47 [attribute-payload]: illegal payload for attribute 'implicit_kind'.
implicit_kind attribute expects: ('var1 : jkind1) * ('var2 : jkind2) ...

module type S4 = sig val a : 'a -> 'b end
|}]


(* Mod bounds. *)

module type S5 = sig
  [@@@implicit_kind: ('a : value mod external_)]

  val e : 'a array -> int
end
[%%expect{|
module type S5 = sig val e : ('a : value mod external_). 'a array -> int end
|}]

(* Annotations must be lr-jkinds: *)
module type S6 = sig
  [@@@implicit_kind: ('no : immutable_data with int -> int)]

  val x : unit -> 'no
end
[%%expect{|
Line 2, characters 48-58:
2 |   [@@@implicit_kind: ('no : immutable_data with int -> int)]
                                                    ^^^^^^^^^^
Error: 'with' syntax is not allowed on a right mode.
|}]


(* Multiple implicit kinds. *)

module type S7 = sig
  [@@@implicit_kind: ('a : immediate) * ('b : bits64)]

  val f : 'a -> 'b -> #('a * 'b)
end

[%%expect{|
module type S7 =
  sig val f : ('a : immediate) ('b : bits64). 'a -> 'b -> #('a * 'b) end
|}]

(* Implicit kinds in type constructors. *)

module type S8 = sig
  [@@@implicit_kind: ('t : word & value)]

  type 't t = { x : 't }
  val f : 'a t -> 'a
end

[%%expect{|
module type S8 =
  sig
    type ('t : word & value) t = { x : 't; }
    val f : ('a : word & value). 'a t -> 'a
  end
|}]

(* Implicit kinds override inference defaults. *)

module type S9 = sig
  [@@@implicit_kind: ('b : bits64)]

  val f : 'a array -> 'a
  val g : 'b array -> 'b
end

[%%expect{|
module type S9 =
  sig val f : 'a array -> 'a val g : ('b : bits64). 'b array -> 'b end
|}]

(* Implicit kinds in nested signatures. *)

module type S10 = sig
  [@@@implicit_kind: ('outer : immediate)]

  module M : sig
    [@@@implicit_kind: ('inner : bits64)]

    val f : 'outer -> 'inner -> 'outer * 'inner array
  end
end

[%%expect{|
module type S10 =
  sig
    module M :
      sig
        val f :
          ('outer : immediate) ('inner : bits64).
            'outer -> 'inner -> 'outer * 'inner array
      end
  end
|}]

(* Implicit kinds with module type inclusion. *)

module type Base = sig
  [@@@implicit_kind: ('t : bits64)]

  val process : 't -> 't
end

module type Extended = sig
  include Base

  val transform : 't -> 't array
end

[%%expect{|
module type Base = sig val process : ('t : bits64). 't -> 't end
module type Extended =
  sig
    val process : ('t : bits64). 't -> 't
    val transform : 't -> 't array
  end
|}]

(* Implicit kinds with structures defined in functors. *)

module type S11 = sig
  [@@@implicit_kind: ('elem : immediate)]

  module F : functor (X : sig val x : 'elem end) -> sig
    val get : unit -> 'elem
  end
end

[%%expect{|
module type S11 =
  sig
    module F :
      functor (X : sig val x : ('elem : immediate). 'elem end) ->
        sig val get : ('elem : immediate). unit -> 'elem end
  end
|}]

(* Multiple variable attributes - latter overrides former. *)

module type S12 = sig
  [@@@implicit_kind: ('a : immediate)]
  [@@@implicit_kind: ('a : bits64)]

  val f : 'a -> 'a
end

[%%expect{|
module type S12 = sig val f : ('a : bits64). 'a -> 'a end
|}]

(* No override when variable names don't match. *)

module type S13 = sig
  [@@@implicit_kind: ('x : immediate)]
  [@@@implicit_kind: ('y : bits64)]

  val f : 'x -> 'y -> 'x * 'y array
end

[%%expect{|
module type S13 =
  sig val f : ('x : immediate) ('y : bits64). 'x -> 'y -> 'x * 'y array end
|}]

(* Multiple variables with partial override.*)

module type S14 = sig
  [@@@implicit_kind: ('a : float32) * ('b : bits64)]
  [@@@implicit_kind: ('b : immediate)]

  val f : 'a -> 'b -> 'b list
end

[%%expect{|
module type S14 =
  sig val f : ('a : float32) ('b : immediate). 'a -> 'b -> 'b list end
|}]

(* Override in nested modules. *)

module type S15 = sig
  [@@@implicit_kind: ('t : bits64)]

  val outer : 't -> 't

  module Inner : sig
    [@@@implicit_kind: ('t : immediate)]

    val inner : 't -> 't
  end
end

[%%expect{|
module type S15 =
  sig
    val outer : ('t : bits64). 't -> 't
    module Inner : sig val inner : ('t : immediate). 't -> 't end
  end
|}]

(* Multiple overrides with different variables. *)

module type S16 = sig
  [@@@implicit_kind: ('a : immediate)]
  [@@@implicit_kind: ('b : bits64)]
  [@@@implicit_kind: ('a : word) * ('c : immediate)]

  val f : 'a -> 'b -> 'c -> 'b array
  val g : 'a -> 'c list
end

[%%expect{|
module type S16 =
  sig
    val f :
      ('a : word) ('b : bits64) ('c : immediate). 'a -> 'b -> 'c -> 'b array
    val g : ('a : word) ('c : immediate). 'a -> 'c list
  end
|}]

(* Annotations narrowing the jkind fail.*)

module type S17 = sig
  [@@@implicit_kind: ('a : value mod immutable)]
  val i : ('a : value mod external_) -> 'a
end

[%%expect{|
Line 3, characters 10-36:
3 |   val i : ('a : value mod external_) -> 'a
              ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was declared to have kind value
                                                                  mod
                                                                  immutable.
       But it was inferred to have kind value mod immutable external_
         because of the annotation on the type variable 'a.
|}]

(* Annotations extending the jkind pass. *)

module type S18 = sig
  [@@@implicit_kind: ('a : value mod immutable)]

  val i : ('a : value) -> 'a
end

[%%expect{|
module type S18 = sig val i : ('a : value mod immutable). 'a -> 'a end
|}]

(* Quantification overrides the default. *)

module type S19 = sig
  [@@@implicit_kind: ('a : value mod immutable)]

  val j : ('a : value mod stateless) . 'b -> 'a
end

[%%expect{|
Line 4, characters 12-13:
4 |   val j : ('a : value mod stateless) . 'b -> 'a
                ^
Error: The type variable 'a has conflicting kind annotations.
       It has an explicit annotation value mod stateless
       but was already implicitly annotated with value mod immutable
|}]

(* Type constructors can't narrow the default: *)

module type S20 = sig
  [@@@implicit_kind: ('a : value_or_null) * ('c : immediate)]

  type ('b : value_or_null mod immutable) t
  val get : 'a t -> 'a
  val geti : 'c t -> 'c
end

[%%expect{|
Line 5, characters 12-14:
5 |   val get : 'a t -> 'a
                ^^
Error: The universal type variable 'a was declared to have kind value_or_null.
       But it was inferred to have kind value_or_null mod immutable
         because of the definition of t at line 4, characters 2-43.
|}]

(* Implicit jkinds in [as] assignments. *)

module type S21 = sig
  [@@@implicit_kind: ('a : bits32)]

  val how : ([> `ignored] as 'a) -> 'a
end

[%%expect{|
Line 4, characters 30-31:
4 |   val how : ([> `ignored] as 'a) -> 'a
                                  ^
Error: This alias is bound to type "[> `ignored ]"
       but is used as an instance of type "('a : bits32)"
       The layout of [> `ignored ] is value
         because it's a polymorphic variant type.
       But the layout of [> `ignored ] must be a sublayout of bits32
         because of the annotation on the implicit kind of type variables named a.
|}]

(* Implicits work in constraints. *)

module type S22 = sig
  [@@@implicit_kind: ('b : bits32)]

  type 'a t constraint 'a = #('b * 'b)

  val f : 'a t -> 'a
end

[%%expect{|
module type S22 =
  sig
    type 'a t constraint 'a = #('b * 'b)
    val f : ('a : bits32). #('a * 'a) t -> #('a * 'a)
  end
|}]

(* Implicits work in GADT constructors. *)

module type S23 = sig
  [@@@implicit_kind: ('a : float64)]

  type exs = | T : 'a -> exs
end

[%%expect{|
module type S23 = sig type exs = T : ('a : float64). 'a -> exs end
|}]

(* Implicits and constrained type constructors. *)

module type S24 = sig
  [@@@implicit_kind: ('null : value_or_null)]

  type 'a t constraint 'a = int

  val f : 'null t -> unit
end

[%%expect{|
module type S24 = sig type 'a t constraint 'a = int val f : int t -> unit end
|}]

(* Quantification without annotations. *)

module type S25 = sig
  [@@@implicit_kind: ('w : word)]

  val f : 'w . 'w -> 'w
end

[%%expect{|
module type S25 = sig val f : ('w : word). 'w -> 'w end
|}]

(* Same in polymorphic record fields. *)

module type S26 = sig
  [@@@implicit_kind: ('w : word)]

  type record = { f : 'w . 'w -> 'w }
end

[%%expect{|
module type S26 = sig type record = { f : ('w : word). 'w -> 'w; } end
|}]

(* Same in polymorphic function arguments. *)

module type S27 = sig
  [@@@implicit_kind: ('w : word)]

  val f : ('w . 'w -> 'w) -> unit
end

[%%expect{|
module type S27 = sig val f : (('w : word). 'w -> 'w) -> unit end
|}]

(* CR implicit-kinds: inherit the annotation here? *)
(* [module type of struct]. *)

module type S28 = sig
  [@@@implicit_kind: ('w : word)]

  module type Evil = module type of struct
    let x (a : 'w) = a
  end
end

[%%expect{|
module type S28 =
  sig module type Evil = sig val x : 'w -> 'w @@ stateless end end
|}]

(* [module type of struct] and unification. *)

module type S29 = sig
  [@@@implicit_kind: ('v : value_or_null)]

  module type VeryEvil = module type of struct
    let x (a : 'v) = a + 2
  end
end

[%%expect{|
module type S29 =
  sig module type VeryEvil = sig val x : int -> int @@ portable end end
|}]

(* CR implicit-kinds: implement in structures. *)
(* Unification in structures. *)

module M2 = struct
  [@@@implicit_kind: ('v : value_or_null)]

  let x (a : 'v) = a + 3
end

[%%expect{|
module M2 : sig val x : int -> int end
|}]

(* Impossible constraints. *)

module type S30 = sig
  [@@@implicit_kind: ('w : word)]

  type 'w t constraint 'w = int
end

[%%expect{|
Line 4, characters 23-31:
4 |   type 'w t constraint 'w = int
                           ^^^^^^^^
Error: The type constraints are not consistent.
       Type "('w : word)" is not compatible with type "int"
       The layout of int is value
         because it is the primitive type int.
       But the layout of int must be a sublayout of word
         because of the annotation on the implicit kind of type variables named w.
|}]

(* Possible constraints. *)
module type S31 = sig
  [@@@implicit_kind: ('v : value_or_null)]

  type 'v t constraint 'v = int
end

[%%expect{|
module type S31 = sig type 'a t constraint 'a = int end
|}]
