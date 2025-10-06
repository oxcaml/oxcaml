(* TEST
 expect;
*)

#syntax quotations on

type s0 = <[int]>;;
[%%expect {|
type s0 = <[int]>
|}];;

type s1 = <[string]>;;
[%%expect {|
type s1 = <[string]>
|}];;

type 'a s2 = <[$'a -> int]> expr;;
[%%expect {|
type 'a s2 = <[$('a) -> int]> expr
|}];;

type ('a, 'b) s3 = <[$'a -> $'b -> $'a * $'b]> expr;;
[%%expect {|
type ('a, 'b) s3 = <[$('a) -> $('b) -> $('a) * $('b)]> expr
|}];;

type ('a, 'b, 'c) s4 = <[$'a list -> $'b option -> $'c]> expr;;
[%%expect {|
type ('a, 'b, 'c) s4 = <[$('a) list -> $('b) option -> $('c)]> expr
|}];;

type ('a, 'b) s5 = <[$'a -> [`A of 'b]]> expr;;
[%%expect {|
Line 32, characters 35-37:
32 | type ('a, 'b) s5 = <[$'a -> [`A of 'b]]> expr;;
                                        ^^
Error: Type variable "b" is used at Line 32, characters 35-37
       in a context with one layer of quotation (<[ ... ]>);
       it should only be used in a context with no quotations or splices.
|}];;

type s6 = <[string -> bool -> [`A | `B of string]]> expr;;
[%%expect {|
type s6 = <[string -> bool -> [ `A | `B of string ]]> expr
|}];;

type 'a t1 = 'a expr;;
[%%expect {|
type 'a t1 = 'a expr
|}];;

type 'a t2 = <['a]> expr;;
[%%expect {|
Line 52, characters 15-17:
52 | type 'a t2 = <['a]> expr;;
                    ^^
Error: Type variable "a" is used at Line 52, characters 15-17
       in a context with one layer of quotation (<[ ... ]>);
       it should only be used in a context with no quotations or splices.
|}];;

type 'a t3 = $'a -> $'a -> 'a expr;;
[%%expect {|
Line 1, characters 27-29:
1 | type 'a t3 = $'a -> $'a -> 'a expr;;
                               ^^
Error: Type variable "'a" is used at Line 1, characters 27-29
       in a context with no quotations or splices;
       it should only be used in a context with one layer of splicing ($).
|}];;

type 'a t4 = $'a -> $'a;;
[%%expect {|
Line 72, characters 14-16:
72 | type 'a t4 = $'a -> $'a;;
                   ^^
Error: Type variable "a" is used at Line 72, characters 14-16
       in a context with one layer of splicing ($);
       it should only be used in a context with no quotations or splices.
|}];;

let f (x : $'a) = x
[%%expect {|
val f : ('a : any). $(<['a]>) -> $(<['a]>) = <fun>
|}];;

let foo1 (x: 'a) = <[fun (y : $'a) -> 1]>;;
[%%expect {|
val foo1 : 'a -> <[$('a) -> int]> expr = <fun>
|}];;

let foo2 (x: 'a) = <[fun (y : 'a) -> 1]>;;
[%%expect {|
Line 1, characters 30-32:
1 | let foo2 (x: 'a) = <[fun (y : 'a) -> 1]>;;
                                  ^^
Error: Type variable "a" is used at Line 1, characters 30-32
       in a context with one layer of quotation (<[ ... ]>);
       it should only be used in a context with no quotations or splices.
|}];;

let foo3 (x: 'a) = <[fun (y : <['a]>) -> 1]>;;
[%%expect {|
Line 102, characters 32-34:
102 | let foo3 (x: 'a) = <[fun (y : <['a]>) -> 1]>;;
                                      ^^
Error: Type variable "a" is used at Line 102, characters 32-34
       in a context with 2 layers of quotation (<[ ... ]>);
       it should only be used in a context with no quotations or splices.
|}];;

let foo4 (x: <['a]> expr) = <[fun (y : 'b) -> ($x, y)]>;;
[%%expect {|
val foo4 : <[$('a)]> expr -> <[$('b) -> $('a) * $('b)]> expr = <fun>
|}];;

let foo5 (x: <['a]> expr) = <[fun (y : 'a) -> ($x, y)]>;;
[%%expect {|
Line 1, characters 39-41:
1 | let foo5 (x: <['a]> expr) = <[fun (y : 'a) -> ($x, y)]>;;
                                           ^^
Error: Type variable "a" is used at Line 1, characters 39-41
       in a context with one layer of quotation (<[ ... ]>);
       it should only be used in a context with no quotations or splices.
|}];;

let foo6 (type a) (type b) x = <[fun (y : a) -> y]>;;
[%%expect {|
Line 1, characters 42-43:
1 | let foo6 (type a) (type b) x = <[fun (y : a) -> y]>;;
                                              ^
Error: Identifier "a" is used at Line 1, characters 42-43
       in a context with one layer of quotation (<[ ... ]>);
       it is introduced at Line 1, characters 15-16
       in a context with no quotations or splices.
|}];;

let foo7 (type a) (type b) x = <[fun (y : $a) -> y]> 42;;
[%%expect {|
Line 1, characters 31-52:
1 | let foo7 (type a) (type b) x = <[fun (y : $a) -> y]> 42;;
                                   ^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "<[$(a) -> $(a)]> expr"
       This is not a function; it cannot be applied.
|}];;

let foo8 (type a) (type b) x = <[fun ((p, q) : $a * $b) -> ($x, (p, q))]> <["foo"]>;;
[%%expect {|
Line 1, characters 31-73:
1 | let foo8 (type a) (type b) x = <[fun ((p, q) : $a * $b) -> ($x, (p, q))]> <["foo"]>;;
                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "<[$(a) * $(b) -> 'a * ($(a) * $(b))]> expr"
       This is not a function; it cannot be applied.
|}];;

(<[fun (y : 'a) -> 1]>, fun (x : 'a) -> ())
[%%expect {|
Line 1, characters 33-35:
1 | (<[fun (y : 'a) -> 1]>, fun (x : 'a) -> ())
                                     ^^
Error: Type variable "a" is used at Line 1, characters 33-35
       in a context with no quotations or splices;
       it should only be used in a context with one layer of quotation (<[ ... ]>).
|}];;

<[fun (type a) (type b) (x : a) (y : b) -> (x, y)]>;;
[%%expect {|
- : <[$('a) -> $('b) -> $('a) * $('b)]> expr =
<[fun (type a) (type b) (x : a) (y : b) -> (x, y)]>
|}];;

type t4 = A | B;;
[%%expect {|
type t4 = A | B
|}];;

<[A]>;;
[%%expect {|
Line 1, characters 2-3:
1 | <[A]>;;
      ^
Error: Constructor "A" used at Line 1, characters 2-3
       is unbound in this context; identifier "A" is unbound
       in a context with one layer of quotation (<[ ... ]>).
|}];;

<[fun (x : 'a) (y : 'b) -> (x, y)]>;;
[%%expect {|
- : <[$('a) -> $('b) -> $('a) * $('b)]> expr =
<[fun (x : 'a) (y : 'b) -> (x, y)]>
|}];;

<[fun (f : 'a. 'a -> 'a) (x : 'b) -> f x]>;;
[%%expect {|
- : <[('a. 'a -> 'a) -> $('b) -> $('b)]> expr =
<[fun (f : 'a. 'a -> 'a) (x : 'b) -> f x]>
|}];;

<[fun (f : 'a. 'a -> 'a) (x : 'a) -> f x]>;;
[%%expect {|
- : <[('a. 'a -> 'a) -> $('a) -> $('a)]> expr =
<[fun (f : 'a. 'a -> 'a) (x : 'a__1) -> f x]>
|}];;

<[fun (x : 'a) (f : 'a. 'a -> 'a) -> f x]>;;
[%%expect {|
- : <[$('a) -> ('a0. 'a0 -> 'a0) -> $('a)]> expr =
<[fun (x : 'a) (f : 'a__1. 'a__1 -> 'a__1) -> f x]>
|}];;

<[fun (f : 'a. 'a -> 'a) (g: 'b 'c. 'b list -> ('b -> 'c) -> 'c list) -> f g]>;;
[%%expect {|
- : <[
     ('a. 'a -> 'a) ->
     ('b 'c. 'b list -> ('b -> 'c) -> 'c list) ->
     $('d) list -> ($('d) -> $('e)) -> $('e) list]>
    expr
=
<[fun (f : 'a. 'a -> 'a) (g : 'b 'c. 'b list -> ('b -> 'c) -> 'c list) -> f g
]>
|}];;

(* The mk_pair examples exist to test whether unification behaves well when
   splices and quotations are present. *)

let mk_pair x = <[$x, $x]>;;
[%%expect {|
val mk_pair : <[$('a)]> expr -> <[$('a) * $('a)]> expr = <fun>
|}];;

mk_pair <[123]>;;
[%%expect {|
- : <[$(<[int]>) * $(<[int]>)]> expr = <[(123, 123)]>
|}];;

mk_pair <[[]]>;;
[%%expect {|
- : <[$(<[$('a) list]>) * $(<[$('a) list]>)]> expr = <[([], [])]>
|}];;

mk_pair <[None]>;;
[%%expect {|
- : <[$(<[$('a) option]>) * $(<[$('a) option]>)]> expr = <[(None, None)]>
|}];;

mk_pair <[Some 123]>;;
[%%expect {|
- : <[$(<[int option]>) * $(<[int option]>)]> expr =
<[((Some 123), (Some 123))]>
|}];;

mk_pair <[fun () -> 42]>;;
[%%expect {|
- : <[$(<[unit -> int]>) * $(<[unit -> int]>)]> expr =
<[((fun () -> 42), (fun () -> 42))]>
|}];;

mk_pair <[fun x -> x]>;;
[%%expect {|
- : <[$(<['_weak1 -> '_weak1]>) * $(<['_weak1 -> '_weak1]>)]> expr =
<[((fun x -> x), (fun x -> x))]>
|}];;

(* Type algebra checks. *)

fun (x: 'a) -> (x: <[<[<[$($($'a))]>]>]>);;
[%%expect {|
- : 'a -> <[<[<[$($($('a)))]>]>]> = <fun>
|}];;

fun (x: <[<[<[$($($'a))]>]>]>) -> (x: 'a);;
[%%expect {|
- : <[<[<[$($($('a)))]>]>]> -> 'a = <fun>
|}];;

fun (x: <[<[<[$($'a)]>]>]>) -> (x: 'a);;
[%%expect {|
Line 1, characters 32-33:
1 | fun (x: <[<[<[$($'a)]>]>]>) -> (x: 'a);;
                                    ^
Error: This expression has type "<['a]>" but an expression was expected of type
         "'a"
       The type variable "'a" occurs inside "<['a]>"
|}];;
