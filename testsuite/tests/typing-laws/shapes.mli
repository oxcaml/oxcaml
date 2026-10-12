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
law? rect (w : float) (h : float) :
  w >= 0. ===> h >= 0. ===> area (Rect { w; h }) = w *. h
law? origin_x (p : point) : { p with x = 0. }.x = origin.x
law? bad (s : string) : (try fail s with Bad s' -> s = s') || Sub.k = 1
law? poly (xs : 'a list) (f : 'a -> 'b) :
  List.length (List.map f xs) = List.length xs
law? variant (v : [ `A | `B of int ]) :
  (match v with `A -> true | `B n -> n = n)
law? disamb (c : t) : (Circle 1. : t) = c || Sub.U = Sub.U
law? optional (f : ?y:int -> x:int -> unit -> int) :
  f ~x:1 () = opt ~x:1 () && f ~y:2 ~x:1 () = opt ~y:2 ~x:1 ()
law? modes (g : local_ int -> int) : apply g = apply g
law? fmt (n : int) : Printf.sprintf "%d" n = string_of_int n
law? fake_format : Format (1, "hello") <> Format (2, "hello")
law? optional_none :
  let pair = (opt ?y:None, ()) in fst pair ~x:1 () = opt ~x:1 ()
law? unboxed (x : float#) : of_unboxed x = of_unboxed x
law? \#type (\#method : int) : \#method = \#method
law? inferred x y : x + y = y + x
