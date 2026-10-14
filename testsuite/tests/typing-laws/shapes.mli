(* Laws exercising the corners of the generated file: local types,
   constructors and labels (also inline records and a constructor name
   shared by two types), exceptions, submodules, polymorphic variants,
   labelled and optional arguments, modes, unboxed types, format strings
   (and a constructor that only looks like a format), type variables, and
   keywords as names. *)

type t = Circle of float | Rect of { w : float; h : float }
type t2 = Circle of int
type point = { x : float; y : float }
type format6 = Format of int * string
exception Bad of string
module Sub : sig val k : int type u = U end
val area : t -> float
val origin : point
val fail : string -> 'a
val opt : ?y:int -> x:int -> unit -> int
val apply : (local_ int -> int) -> int
val of_unboxed : float# -> float

law? area_nonneg (s : t) : area s >= 0.
law? poly (xs : 'a list) (f : 'a -> 'b) :
  List.length (List.map f xs) = List.length xs
law? modes (g : local_ int -> int) : apply g = apply g
law? unboxed (x : float#) : of_unboxed x = of_unboxed x
law? \#type (\#method : int) : \#method = \#method
law? inferred x y : x + y = y + x
