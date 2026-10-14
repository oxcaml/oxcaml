(* TEST
 flags = "-extension laws";
 expect;
*)

(* [-i] prints laws with the types of their parameters and their clauses.
   Applications are parenthesized so that the output parses again. *)

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
    law? roundtrip (x : t) (f : int -> t) : (to_int (create 1)) = 1 ===>
      (to_int (f 1)) >= 0 ===> (to_int x) = (to_int x)
  end
|}]

(* The kind of a type variable shared by the parameters is printed at its
   first occurrence. *)

module type Kinded = sig
  val f : ('a : immutable_data) -> bool
  law? kinded (x : ('a : immutable_data)) (y : 'a) : f x && f y
  law? kinded_unused (x : ('a : immutable_data)) : 1 = 1
end
[%%expect {|
module type Kinded =
  sig
    val f : ('a : immutable_data). 'a -> bool
    law? kinded (x : ('a : immutable_data)) (y : 'a) : (f x) && (f y)
    law? kinded_unused (x : ('a : immutable_data)) : 1 = 1
  end
|}]

(* A user constructor named [Format] is not mistaken for a format string. *)

module type Fake_format = sig
  type format6 = Format of int * string
  law? distinct : Format (1, "hello") <> Format (2, "hello")
end
[%%expect {|
module type Fake_format =
  sig
    type format6 = Format of int * string
    law? distinct :
      (Format (1, "hello") : format6) <> (Format (2, "hello") : format6)
  end
|}]

(* Constructors are annotated with their types, so that the printed
   signature types again with the same constructors when several types
   share them. *)

module type Disambiguated = sig
  type t = Circle of float
  type t2 = Circle of int
  law? circle (c : t) : (Circle 1. : t) = c
end
[%%expect {|
module type Disambiguated =
  sig
    type t = Circle of float
    type t2 = Circle of int
    law? circle (c : t) : (Circle 1. : t) = c
  end
|}]

(* So are records and field accesses, for their labels. *)

module type Disambiguated_record = sig
  type r = { x : int }
  type r2 = { x : float }
  law? record (r : r) : ({ x = 1 } : r) = r && (r : r).x = 1
end
[%%expect {|
module type Disambiguated_record =
  sig
    type r = { x : int; }
    type r2 = { x : float; }
    law? record (r : r) : (({ x = 1 } : r) = r) && ((r : r).x = 1)
  end
|}]

(* The [None] the type checker inserts for an omitted optional argument is
   printed like an explicit [?x:None]. *)

module type Optional = sig
  val f : ?x:int -> unit -> int
  law? explicit : f ?x:None () = 0
  law? omitted : f () = 0
end
[%%expect {|
module type Optional =
  sig
    val f : ?x:int -> unit -> int
    law? explicit : (f ?x:None ()) = 0
    law? omitted : (f ?x:None ()) = 0
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
    law? \#end : (\#method 1) = (\#method 1)
  end
|}]

(* The constructors of a type through a functor application, as [with
   module M := F (X)] introduces, have no qualified syntax: they are
   printed unqualified, and the type annotation resolves them when the
   printed law is typed again. *)

module Applied_functor (X : sig end) : sig
  type t = A | B
  module N : sig type u = C end
end = struct
  type t = A | B
  module N = struct type u = C end
end
module Arg = struct end
module type Applied_sig = sig
  module M : sig
    type t = A | B
    module N : sig type u = C end
  end
  law? m (x : M.t) : x = M.A
  law? n (y : M.N.u) : y = M.N.C
end
module type Applied = Applied_sig with module M := Applied_functor (Arg)
[%%expect {|
module Applied_functor :
  functor (X : sig end) ->
    sig type t = A | B module N : sig type u = C end end
module Arg : sig end
module type Applied_sig =
  sig
    module M : sig type t = A | B module N : sig type u = C end end
    law? m (x : M.t) : x = (M.A : M.t)
    law? n (y : M.N.u) : y = (M.N.C : M.N.u)
  end
module type Applied =
  sig
    law? m (x : Applied_functor(Arg).t) : x = (A : Applied_functor(Arg).t)
    law? n (y : Applied_functor(Arg).N.u) :
      y = (C : Applied_functor(Arg).N.u)
  end
|}]

module Printed : Applied = struct
  law? m (x : Applied_functor(Arg).t) : x = (A : Applied_functor(Arg).t)
  law? n (y : Applied_functor(Arg).N.u) : y = (C : Applied_functor(Arg).N.u)
end
[%%expect {|
module Printed : Applied
|}]
