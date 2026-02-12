#syntax quotations on

type rcd1 = { f : 'a. 'a -> 'a }
type tr1 = rcd1 -> int * string


type obj1 = < f : 'a. 'a -> 'a >
type to1 = obj1 -> int * string

type obj2 = < f : 'a 'b. 'a -> 'b -> 'a >
type to2 = obj2 -> int * string

type obj3 = < f : 'a. 'a -> int -> [ `Foo of <foo: 'a> ] * int >
type to3 = obj3 -> ([ `Foo of <foo: int> ] * int)
                 * ([ `Foo of <foo: string> ] * int)

type obj4 = < f : ('a : immediate). 'a -> 'a >
type to4 = obj4 -> int * bool

type objr = < f : 'a. 'a -> 'b -> 'a; x : int > as 'b
type tr = objr -> int * string


type t1 = ('a. 'a -> 'a) -> int * string

type t2 = ('a 'b. 'a -> 'b -> 'a) -> int * string

type t3 =
  ('a. 'a -> int -> [ `Foo of <foo: 'a> ] * int) ->
  ([ `Foo of <foo: int> ] * int) * ([ `Foo of <foo: string> ] * int)

type t3' =
  ('a. 'a -> int -> [ `Foo of 'a ] * int) ->
  ([ `Foo of int ] * int) * ([ `Foo of string ] * int)

type t3'' =
  ('a 'b. unit -> ('a expr -> 'b expr) -> <[unit -> $('a) -> $('b)]> expr) ->
  <[unit -> int -> int]> expr * <[unit -> int -> string]> expr

type 'b tbar = [`Bar of 'b]

module type T = sig
  type t
  val a : t
end

type t3''' =
  ('a. 'a -> (module T with type t = 'a)) ->
  (module T with type t = int) * (module T with type t = string)

type t4 = (('a : immediate). 'a -> 'a) -> int * bool


type te1 = t1 -> int * string

type te2 = t2 -> int * string

type te3' = t3' -> ([ `Foo of int ] * int) * ([ `Foo of string ] * int)

type te4 = t4 -> int * bool
