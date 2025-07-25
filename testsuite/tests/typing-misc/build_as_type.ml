(* TEST
   expect;
*)

let f = function ([] : int list) as x -> x | _ :: _ -> assert false

[%%expect {|
val f : int list -> int list = <fun>
|}]

let f =
  let f' = function ([] : 'a list) as x -> x | _ :: _ -> assert false in
  f', f'

[%%expect
{|
val f : ('a list -> 'a list) * ('a list -> 'a list) = (<fun>, <fun>)
|}]

let f =
  let f' = function ([] : _ list) as x -> x | _ :: _ -> assert false in
  f', f'

[%%expect
{|
val f : ('a list -> 'b list) * ('c list -> 'd list) = (<fun>, <fun>)
|}]

let f =
  let f' (type a) = function
    | ([] : a list) as x -> x
    | _ :: _ -> assert false
  in
  f', f'

[%%expect
{|
val f : ('a list -> 'a list) * ('b list -> 'b list) = (<fun>, <fun>)
|}]

type t =
  [ `A
  | `B ]

[%%expect {|
type t = [ `A | `B ]
|}]

let f = function `A as x -> x | `B -> `A

[%%expect {|
val f : [< `A | `B ] -> [> `A ] = <fun>
|}]

let f = function (`A : t) as x -> x | `B -> `A

[%%expect {|
val f : t -> t = <fun>
|}]

let f : t -> _ = function `A as x -> x | `B -> `A

[%%expect {|
val f : t -> [> `A ] = <fun>
|}]

let f = function
  | (`A : t) as x -> (
    (* This should be flagged as non-exhaustive: because of the constraint [x]
       is of type [t]. *)
    match x with `A -> ())
  | `B -> ()

[%%expect
{|
Lines 2-5, characters 21-26:
2 | .....................(
3 |     (* This should be flagged as non-exhaustive: because of the constraint [x]
4 |        is of type [t]. *)
5 |     match x with `A -> ())
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
`B

val f : t -> unit = <fun>
|}]

let f = function
  | (`A : t) as x -> ( match x with `A -> () | `B -> ())
  | `B -> ()

[%%expect {|
val f : t -> unit = <fun>
|}]

let f = function
  | (`A : t) as x -> ( match x with `A -> () | `B -> () | `C -> ())
  | `B -> ()

[%%expect
{|
Line 2, characters 58-60:
2 |   | (`A : t) as x -> ( match x with `A -> () | `B -> () | `C -> ())
                                                              ^^
Error: This pattern matches values of type "[? `C ]"
       but a pattern was expected which matches values of type "t"
       The second variant type does not allow tag(s) "`C"
|}]

let f = function ((`A, _) : _ * int) as x -> x

[%%expect {|
val f : [< `A ] * int -> [> `A ] * int = <fun>
|}]

(* Make sure *all* the constraints are respected: *)

let f = function
  | ((`A : _) : t) as x -> (
    (* This should be flagged as non-exhaustive: because of the constraint [x]
       is of type [t]. *)
    match x with `A -> ())
  | `B -> ()

[%%expect
{|
Lines 2-5, characters 27-26:
2 | ...........................(
3 |     (* This should be flagged as non-exhaustive: because of the constraint [x]
4 |        is of type [t]. *)
5 |     match x with `A -> ())
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
`B

val f : t -> unit = <fun>
|}]

let f = function
  | ((`A : t) : _) as x -> (
    (* This should be flagged as non-exhaustive: because of the constraint [x]
       is of type [t]. *)
    match x with `A -> ())
  | `B -> ()

[%%expect
{|
Lines 2-5, characters 27-26:
2 | ...........................(
3 |     (* This should be flagged as non-exhaustive: because of the constraint [x]
4 |        is of type [t]. *)
5 |     match x with `A -> ())
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
`B

val f : t -> unit = <fun>
|}]
