(* TEST
 flags = "-w +53";
 expect;
*)

(* Variable defaults in signatures: *)

module type S0 = sig
  [@@@variable: ('elt : bits64)]

  val f : 'elt -> 'elt array
end

[%%expect{|
module type S0 = sig val f : ('elt : bits64). 'elt -> 'elt array end
|}]

(* Variable defaults do not work in structures: *)

module M = struct
  [@@@variable: ('elt : bits64)]

  let f : 'elt -> 'elt array = fun x -> [| x |]
end

[%%expect{|
module M : sig val f : 'elt -> 'elt array end
|}]

(* Variable without jkind annotation should fail: *)

module type S1 = sig
  [@@@variable: 'a]
end

[%%expect{|
Line 2, characters 2-19:
2 |   [@@@variable: 'a]
      ^^^^^^^^^^^^^^^^^
Warning 47 [attribute-payload]: illegal payload for attribute 'variable'.
variable attribute expects: ('var1 : jkind1) * ('var2 : jkind2) ...

module type S1 = sig end
|}]

(* Invalid syntax - not a type variable: *)

module type S2 = sig
  [@@@variable: int]
end

[%%expect{|
Line 2, characters 2-20:
2 |   [@@@variable: int]
      ^^^^^^^^^^^^^^^^^^
Warning 47 [attribute-payload]: illegal payload for attribute 'variable'.
variable attribute expects: ('var1 : jkind1) * ('var2 : jkind2) ...

module type S2 = sig end
|}]

(* Mod bounds. *)

module type S3 = sig
  [@@@variable: ('a : value mod external_)]

  val e : 'a array -> int
end
[%%expect{|
module type S3 = sig val e : ('a : value mod external_). 'a array -> int end
|}]

(* Must be an lr-jkind: *)
module type S4 = sig
  [@@@variable: ('no : immutable_data with int -> int)]

  val x : unit -> 'no
end
[%%expect{|
Line 2, characters 43-53:
2 |   [@@@variable: ('no : immutable_data with int -> int)]
                                               ^^^^^^^^^^
Error: 'with' syntax is not allowed on a right mode.
|}]


(* Multiple variable defaults: *)

module type S5 = sig
  [@@@variable: ('a : immediate) * ('b : bits64)]

  val f : 'a -> 'b -> #('a * 'b)
end

[%%expect{|
module type S5 =
  sig val f : ('a : immediate) ('b : bits64). 'a -> 'b -> #('a * 'b) end
|}]

(* Variable defaults in type constructors: *)

module type S6 = sig
  [@@@variable: ('t : word & value)]

  type 't t = { x : 't }
  val f : 'a t -> 'a
end

[%%expect{|
module type S6 =
  sig
    type ('t : word & value) t = { x : 't; }
    val f : ('a : word & value). 'a t -> 'a
  end
|}]

(* Variable defaults override inference defaults: *)

module type S7 = sig
  [@@@variable: ('b : bits64)]

  val f : 'a array -> 'a
  val g : 'b array -> 'b
end

[%%expect{|
module type S7 =
  sig val f : 'a array -> 'a val g : ('b : bits64). 'b array -> 'b end
|}]

(* Variable defaults in nested signatures: *)

module type S8 = sig
  [@@@variable: ('outer : immediate)]

  module M : sig
    [@@@variable: ('inner : bits64)]

    val f : 'outer -> 'inner -> 'outer * 'inner array
  end
end

[%%expect{|
module type S8 =
  sig
    module M :
      sig
        val f :
          ('outer : immediate) ('inner : bits64).
            'outer -> 'inner -> 'outer * 'inner array
      end
  end
|}]

(* Variable defaults with module type inclusion: *)

module type Base = sig
  [@@@variable: ('t : bits64)]

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

(* Variable defaults with structures defined in functors: *)

module type S9 = sig
  [@@@variable: ('elem : immediate)]

  module F : functor (X : sig val x : 'elem end) -> sig
    val get : unit -> 'elem
  end
end

[%%expect{|
module type S9 =
  sig
    module F :
      functor (X : sig val x : ('elem : immediate). 'elem end) ->
        sig val get : ('elem : immediate). unit -> 'elem end
  end
|}]

(* Multiple variable attributes - later ones override earlier ones: *)

module type S10 = sig
  [@@@variable: ('a : immediate)]
  [@@@variable: ('a : bits64)]

  val f : 'a -> 'a
end

[%%expect{|
module type S10 = sig val f : ('a : bits64). 'a -> 'a end
|}]

(* No override when variable names don't match: *)

module type S11 = sig
  [@@@variable: ('x : immediate)]
  [@@@variable: ('y : bits64)]

  val f : 'x -> 'y -> 'x * 'y array
end

[%%expect{|
module type S11 =
  sig val f : ('x : immediate) ('y : bits64). 'x -> 'y -> 'x * 'y array end
|}]

(* Multiple variables with partial override: *)

module type S12 = sig
  [@@@variable: ('a : immediate) * ('b : bits64)]
  [@@@variable: ('b : immediate)]

  val f : 'a -> 'b -> 'a * 'b list
end

[%%expect{|
module type S12 =
  sig val f : ('a : immediate) ('b : immediate). 'a -> 'b -> 'a * 'b list end
|}]

(* Override in nested modules: *)

module type S13 = sig
  [@@@variable: ('t : bits64)]

  val outer : 't -> 't

  module Inner : sig
    [@@@variable: ('t : immediate)]

    val inner : 't -> 't
  end
end

[%%expect{|
module type S13 =
  sig
    val outer : ('t : bits64). 't -> 't
    module Inner : sig val inner : ('t : immediate). 't -> 't end
  end
|}]

(* Multiple overrides with different variables: *)

module type S14 = sig
  [@@@variable: ('a : immediate)]
  [@@@variable: ('b : bits64)]
  [@@@variable: ('a : word) * ('c : immediate)]

  val f : 'a -> 'b -> 'c -> 'b array
  val g : 'a -> 'c list
end

[%%expect{|
module type S14 =
  sig
    val f :
      ('a : word) ('b : bits64) ('c : immediate). 'a -> 'b -> 'c -> 'b array
    val g : ('a : word) ('c : immediate). 'a -> 'c list
  end
|}]

(* Annotations narrow the default, explicit quantification overrides it: *)

module type S15 = sig
  [@@@variable: ('a : value mod immutable)]

  val h : 'a -> 'a
  val i : ('a : value mod external_) -> 'a

  val j : ('a : value mod stateless) . 'b -> 'a
end

[%%expect{|
module type S15 =
  sig
    val h : ('a : value mod immutable). 'a -> 'a
    val i : ('a : value mod immutable external_). 'a -> 'a
    val j : 'b ('a : value mod stateless). 'b -> 'a
  end
|}]

(* Type inference can narrow the default: *)

module type S16 = sig
  [@@@variable: ('a : value_or_null) * ('c : immediate)]

  type ('b : value_or_null mod immutable) t
  val get : 'a t -> 'a
  val geti : 'c t -> 'c
end

[%%expect{|
module type S16 =
  sig
    type ('b : value_or_null mod immutable) t
    val get : ('a : value_or_null mod immutable). 'a t -> 'a
    val geti : ('c : immediate). 'c t -> 'c
  end
|}]

(* Underscore is unimplemented syntax. *)

module type S17 = sig
  [@@@variable: ('a : _)]

  val x : 'a -> 'a
end

[%%expect{|
Line 2, characters 22-23:
2 |   [@@@variable: ('a : _)]
                          ^
Error: Unimplemented kind syntax
|}]

(* The default is ignored in [as] assignments -- it's always [value]. *)

module type S18 = sig
  [@@@variable: ('a : bits32)]

  val how : ([> `ignored] as 'a) -> 'a
end

[%%expect{|
module type S18 = sig val how : ([> `ignored ] as 'a) -> 'a end
|}]

(* Defaults work in constraints. *)

module type S19 = sig
  [@@@variable: ('b : bits32)]

  type 'a t constraint 'a = #('b * 'b)

  val f : 'a t -> 'a
end

[%%expect{|
module type S19 =
  sig
    type 'a t constraint 'a = #('b * 'b)
    val f : ('a : bits32). #('a * 'a) t -> #('a * 'a)
  end
|}]

(* Defaults work in GADT constructors. *)

module type S20 = sig
  [@@@variable: ('a : float64)]

  type exs = | T : 'a -> exs
end

[%%expect{|
module type S20 = sig type exs = T : ('a : float64). 'a -> exs end
|}]
