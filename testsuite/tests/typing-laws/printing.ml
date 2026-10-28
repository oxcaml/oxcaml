(* TEST
 flags = "-extension laws -extension small_numbers";
 expect;
*)

(* [-i] prints laws with the types of their parameters and their clauses. *)

module type Basic = sig
  type t
  val create : int -> t
  val to_int : t -> int
  law? roundtrip (x : t) (f : int -> t) :
    to_int (create 1) = 1 ===> to_int (f 1) >= 0 ===> to_int x = to_int x
end
[%%expect {|
module type Basic =
  sig
    type t
    val create : int -> t
    val to_int : t -> int
    law? roundtrip (x : t) (f : int -> t) : (to_int (create (1))) = (1) ===>
      (to_int (f (1))) >= (0) ===> (to_int x) = (to_int x)
  end
|}]

(* The kind of a type variable shared by the parameters is printed at its
   first occurrence, and the one of a type of the law where it is bound. *)

module type Kinded = sig
  val f : ('a : immutable_data) -> bool
  law? kinded (x : ('a : immutable_data)) (y : 'a) : f x && f y
  law? kinded_unused (x : ('a : immutable_data)) : 1 = 1
  law? plain (xs : 'a list) : 1 = 1
  law? types (type a (b : value_or_null)) (x : a) (y : b) (z : 'a) : 1 = 1
end
[%%expect {|
module type Kinded =
  sig
    val f : ('a : immutable_data). 'a -> bool
    law? kinded (x : ('a : immutable_data)) (y : 'a) : (f x) && (f y)
    law? kinded_unused (x : ('a : immutable_data)) : (1) = (1)
    law? plain (xs : 'a list) : (1) = (1)
    law? types (type a) (type (b : value_or_null)) (x : a) (y : b)
      (z : 'a0) : (1) = (1)
  end
|}]

(* Names that are keywords are printed escaped, for laws and parameters. *)

module type Raw_names = sig
  val \#method : int -> int
  law? \#type (\#method : int) : \#method = \#method
  law? \#end : \#method 1 = \#method 1
end
[%%expect {|
module type Raw_names =
  sig
    val \#method : int -> int
    law? \#type (\#method : int) : \#method = \#method
    law? \#end : (\#method (1)) = (\#method (1))
  end
|}]

(* Constants are printed as written. *)

module type Constants = sig
  val boxed : char -> string -> float -> bool
  val sized : int8 -> int16 -> int32 -> int64 -> nativeint -> bool
  val unboxed : float# -> bool
  law? boxed : boxed 'a' {|b|} (-0.)
  law? sized : sized 1s 2S 3l (-4L) 5n
  law? unboxed : unboxed (-#1.5)
end
[%%expect {|
module type Constants =
  sig
    val boxed : char -> string -> float -> bool
    val sized : int8 -> int16 -> int32 -> int64 -> nativeint -> bool
    val unboxed : float# -> bool
    law? boxed : boxed ('a') ("b") (-0.)
    law? sized : sized (1s) (2S) (3l) (-4L) (5n)
    law? unboxed : unboxed (-#1.5)
  end
|}]

(* Tuples, variants, unboxed values and extension constructors. *)

module type Structures = sig
  val f : int * string -> [ `A of int | `B ] -> bool
  val g : #(int * bool) -> unit# -> bool# -> extension_constructor -> bool
  law? tuple (x : int) : f (x, "s") (`A x) && f (0, "") `B
  law? labelled (t : x:int * string) : t = (~x:1, "s")
  law? unboxed : g #(1, true) #() #false [%extension_constructor Not_found]
end
[%%expect {|
module type Structures =
  sig
    val f : int * string -> [ `A of int | `B ] -> bool
    val g : #(int * bool) -> unit# -> bool# -> extension_constructor -> bool
    law? tuple (x : int) : (f (x, ("s")) (`A x)) && (f ((0), ("")) `B)
    law? labelled (t : x:int * string) : t = (~x:(1), ("s"))
    law? unboxed :
      g #((1), (true : bool)) #() #false [%extension_constructor Not_found]
  end
|}]

(* Constructors of other than predefined types are annotated with their
   types, so that the printed signature types again with the same
   constructors when several types share them. *)

module type Disambiguated = sig
  type t = Circle of float
  type t2 = Circle of int
  law? circle (c : t) : (Circle 1. : t) = c && Some c <> None
end
[%%expect {|
module type Disambiguated =
  sig
    type t = Circle of float
    type t2 = Circle of int
    law? circle (c : t) :
      ((Circle (1.) : t) = c) && ((Some c : _ option) <> (None : _ option))
  end
|}]

(* The [None] that the type checker inserts for an omitted optional argument
   is printed like an explicit [?x:None]. *)

module type Optional = sig
  val f : ?x:int -> unit -> int
  law? omitted : f () = 0
end
[%%expect {|
module type Optional =
  sig
    val f : ?x:int -> unit -> int
    law? omitted : (f ?x:(None : _ option) (() : unit)) = (0)
  end
|}]

(* The constructors of a type through a functor application, as
   [with module M := F (X)] introduces, have no qualified syntax: they are
   printed unqualified, and the annotation resolves them when the printed
   law is typed again. *)

module Applied_functor (X : sig end) : sig
  type t = A | B
end = struct
  type t = A | B
end
module Arg = struct end
module type Applied_sig = sig
  module M : sig type t = A | B end
  law? m (x : M.t) : x = M.A
end
module type Applied = Applied_sig with module M := Applied_functor (Arg)
module Printed : Applied = struct
  law? m (x : Applied_functor(Arg).t) : x = (A : Applied_functor(Arg).t)
end
[%%expect {|
module Applied_functor : functor (X : sig end) -> sig type t = A | B end
module Arg : sig end
module type Applied_sig =
  sig module M : sig type t = A | B end law? m (x : M.t) : x = (A : M.t) end
module type Applied =
  sig
    law? m (x : Applied_functor(Arg).t) : x = (A : Applied_functor(Arg).t)
  end
module Printed : Applied
|}]

(* So are records and the records of field accesses, for their labels. *)

module type Disambiguated_record = sig
  type r = { x : int; y : int }
  type r2 = { x : float }
  law? record (r : r) : ({ x = 1; y = 2 } : r) = r && (r : r).x = 1
  law? update (r : r) : ({ r with x = 1 } : r) <> r
end
[%%expect {|
module type Disambiguated_record =
  sig
    type r = { x : int; y : int; }
    type r2 = { x : float; }
    law? record (r : r) :
      (({ x = (1); y = (2) } : r) = r) && (((r : r).x) = (1))
    law? update (r : r) : ({ r with x = (1) } : r) <> r
  end
|}]

(* Unboxed records and their fields. *)

module type Unboxed_record = sig
  type u = #{ a : int; b : int }
  val sum : u -> int
  law? unboxed (u : u) : sum #{ a = 1; b = 2 } = 3 && u.#a = u.#a
end
[%%expect {|
module type Unboxed_record =
  sig
    type u = #{ a : int; b : int; }
    val sum : u -> int
    law? unboxed (u : u) :
      ((sum (#{ a = (1); b = (2) } : u)) = (3)) &&
        (((u : u).#a) = ((u : u).#a))
  end
|}]

(* Conditionals, sequences, assertions and lazy values. *)

module type Control = sig
  val f : int -> int
  val g : unit -> unit
  law? conditional (x : int) : (if x > 0 then f x else 0) >= 0
  law? sequence (x : int) : (g (); f x) = f x
  law? asserted (x : int) : (assert (x = x); true)
  law? lazy_value (x : int) : Lazy.force (lazy (f x)) = f x
end
[%%expect {|
module type Control =
  sig
    val f : int -> int
    val g : unit -> unit
    law? conditional (x : int) : (if x > (0) then (f x) else (0)) >= (0)
    law? sequence (x : int) : ((g (() : unit)); (f x)) = (f x)
    law? asserted (x : int) : ((assert (x = x)); (true : bool))
    law? lazy_value (x : int) : (Lazy.force (lazy (f x))) = (f x)
  end
|}]

(* Let-bindings, pattern matching and exception handlers. *)

type shape = Circle of float | Rect of { w : float; h : float }
module type Binding = sig
  val area : shape -> float
  law? let_ (s : shape) : let a = area s and b = area s in a = b
  law? match_ (s : shape) :
    (match s with
     | Circle r when r > 0. -> area s > 0.
     | Circle _ | Rect { w = 0.; _ } -> true
     | Rect { w; h } as r -> area r = w *. h)
  law? try_ (s : shape) : (try area s >= 0. with Division_by_zero -> true)
end
[%%expect {|
type shape = Circle of float | Rect of { w : float; h : float; }
module type Binding =
  sig
    val area : shape -> float
    law? let_ (s : shape) : let a = area s and b = area s in a = b
    law? match_ (s : shape) :
      match s with
      | (Circle r : shape) when (r > (0.)) -> ((area s) > (0.))
      | (Circle _ : shape) | (Rect { w = (0.); _ } : shape) -> (true : bool)
      | (Rect { w = w; h = h } : shape) as r -> ((area r) = (w *. h))
    law? try_ (s : shape) :
      try (area s) >= (0.) with | Division_by_zero -> (true : bool)
  end
|}]

(* Functions, with labelled and optional parameters, and with cases. *)

module type Functions = sig
  val apply : (x:int -> ?y:int -> int -> int) -> int
  val all : (int -> bool) -> int list -> bool
  law? labelled : apply (fun ~x ?(y = 0) z -> x + y + z) = 1
  law? cases (xs : int list) :
    all (function 0 -> true | n -> n > 0) xs ===> List.for_all (fun x -> x >= 0) xs
end
[%%expect {|
module type Functions =
  sig
    val apply : (x:int -> ?y:int -> int -> int) -> int
    val all : (int -> bool) -> int list -> bool
    law? labelled : (apply (fun ~x:x ?y:(y = (0)) z -> (x + y) + z)) = (1)
    law? cases (xs : int list) :
      all (function | (0) -> (true : bool) | n -> (n > (0))) xs ===>
      List.for_all (fun x -> x >= (0)) xs
  end
|}]

(* A local open of a module path stands for the paths it introduces. *)

module type Local = sig
  module M : sig val x : int end
  law? local_open : M.(x = x)
end
[%%expect {|
module type Local =
  sig module M : sig val x : int end law? local_open : M.x = M.x end
|}]
