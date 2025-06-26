(* TEST
 expect;
*)

type s0 = <[int]>;;
[%%expect {|
type s0 = <[ int ]>
|}];;

type s1 = <[string]>;;
[%%expect {|
type s1 = <[ string ]>
|}];;

type 'a s2 = <[$'a -> int]> expr;;
[%%expect {|
type 'a s2 = <[ $ ('a) -> int ]> expr
|}];;

type ('a, 'b) s3 = <[$'a -> $'b -> $'a * $'b]> expr;;
[%%expect {|
type ('a, 'b) s3 = <[ $ ('a) -> $ ('b) -> $ ('a) * $ ('b) ]> expr
|}];;

type ('a, 'b, 'c) s4 = <[$'a list -> $'b option -> $'c]> expr;;
[%%expect {|
type ('a, 'b, 'c) s4 = <[ $ ('a) list -> $ ('b) option -> $ ('c) ]> expr
|}];;

type ('a, 'b) s5 = <['a -> [> `A of 'b ]]> expr;;
[%%expect {|
Line 30, characters 21-23:
30 | type ('a, 'b) s5 = <['a -> [> `A of 'b ]]> expr;;
                          ^^
Error: Type variable "a" is used at Line 30, characters 21-23
       in a context with one layer of quotation (<[ ... ]>);
       it should only be used in a context with no quotations or splices.
|}];;

type s6 = <[string -> bool -> [> `A | `B of string]]> expr;;
[%%expect {|
Line 1, characters 0-58:
1 | type s6 = <[string -> bool -> [> `A | `B of string]]> expr;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In type "<[ string -> bool -> [> `A | `B of string ] ]> expr"
       the variable "'a" is unbound
|}];;

type 'a t1 = 'a expr;;
[%%expect {|
type 'a t1 = 'a expr
|}];;

type 'a t2 = <['a]> expr;;
[%%expect {|
Line 55, characters 15-17:
55 | type 'a t2 = <['a]> expr;;
                    ^^
Error: Type variable "a" is used at Line 55, characters 15-17
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

fun (x: 'a) -> <[fun (y : $'a) -> 1]>;;
[%%expect {|
- : 'a -> <[ $ ('a) -> int ]> expr = <fun>
|}];;

fun (x: 'a) -> <[fun (y : 'a) -> 1]>;;
[%%expect {|
Line 1, characters 26-28:
1 | fun (x: 'a) -> <[fun (y : 'a) -> 1]>;;
                              ^^
Error: Type variable "a" is used at Line 1, characters 26-28
       in a context with one layer of quotation (<[ ... ]>);
       it should only be used in a context with no quotations or splices.
|}];;

fun (x: 'a) -> <[fun (y : <['a]>) -> 1]>;;
[%%expect {|
Line 90, characters 28-30:
90 | fun (x: 'a) -> <[fun (y : <['a]>) -> 1]>;;
                                 ^^
Error: Type variable "a" is used at Line 90, characters 28-30
       in a context with 2 layers of quotation (<[ ... ]>);
       it should only be used in a context with no quotations or splices.
|}];;

fun (x: <['a]> expr) -> <[fun (y : 'b) -> ($x, y)]>;;
[%%expect {|
- : <[ $ ('a) ]> expr -> <[ $ ('b) -> $ ('a) * $ ('b) ]> expr = <fun>
|}];;

fun (x: <['a]> expr) -> <[fun (y : 'a) -> ($x, y)]>;;
[%%expect {|
Line 1, characters 35-37:
1 | fun (x: <['a]> expr) -> <[fun (y : 'a) -> ($x, y)]>;;
                                       ^^
Error: Type variable "a" is used at Line 1, characters 35-37
       in a context with one layer of quotation (<[ ... ]>);
       it should only be used in a context with no quotations or splices.
|}];;

fun (type a) (type b) x -> <[fun (y : a) -> y]>;;
[%%expect {|
Line 1, characters 38-39:
1 | fun (type a) (type b) x -> <[fun (y : a) -> y]>;;
                                          ^
Error: Identifier "a" is used at Line 1, characters 38-39
       in a context with one layer of quotation (<[ ... ]>);
       it is introduced at Line 1, characters 10-11
       in a context with no quotations or splices.
|}];;

(fun (type a) (type b) x -> <[fun (y : $a) -> y]>) 42;;
[%%expect {|
- : <[ $ ('_a) -> $ ('_a) ]> expr = <[ fun (y : _) -> y ]>
|}];;

(fun (type a) (type b) x -> <[fun ((p, q) : $a * $b) -> ($x, (p, q))]>) <["foo"]>;;
[%%expect {|
- : <[ $ ('_a) * $ ('_b) -> string * ($ ('_a) * $ ('_b)) ]> expr =
<[ fun ((p, q) : _ * _) -> ("foo", (p, q)) ]>
|}];;

<[fun (type a) (type b) (x : a) (y : b) -> (x, y)]>;;
[%%expect {|
- : <[ $ ('a) -> $ ('b) -> $ ('a) * $ ('b) ]> expr =
<[ fun (type a) (type b) (x : a) (y : b) -> (x, y) ]>
|}];;

<[fun (x : 'a) (y : 'b) -> (x, y)]>;;
[%%expect {|
- : <[ $ ('a) -> $ ('b) -> $ ('a) * $ ('b) ]> expr =
<[ fun (x : 'a) (y : 'b) -> (x, y) ]>
|}];;

<[fun (f : 'a. 'a -> 'a) (x : 'b) -> f x]>;;
[%%expect {|
- : <[ ('a. 'a -> 'a) -> $ ('b) -> $ ('b) ]> expr =
<[ fun (f : 'a. 'a -> 'a) (x : 'b) -> f x ]>
|}];;

<[fun (f : 'a. 'a -> 'a) (x : 'a) -> f x]>;;
[%%expect {|
- : <[ ('a. 'a -> 'a) -> $ ('a) -> $ ('a) ]> expr =
<[ fun (f : 'a. 'a -> 'a) (x : 'a__1) -> f x ]>
|}];;

<[fun (x : 'a) (f : 'a. 'a -> 'a) -> f x]>;;
[%%expect {|
- : <[ $ ('a) -> ('a0. 'a0 -> 'a0) -> $ ('a) ]> expr =
<[ fun (x : 'a) (f : 'a__1. 'a__1 -> 'a__1) -> f x ]>
|}];;

<[fun (f : 'a. 'a -> 'a) (g: 'b 'c. 'b list -> ('b -> 'c) -> 'c list) -> f g]>;;
[%%expect {|
- : <[
     ('a. 'a -> 'a) ->
     ('b 'c. 'b list -> ('b -> 'c) -> 'c list) ->
     $ ('d) list -> ($ ('d) -> $ ('e)) -> $ ('e) list ]>
    expr
=
<[
  fun (f : 'a. 'a -> 'a) (g : 'b 'c. 'b list -> ('b -> 'c) -> 'c list) -> f g
]>
|}];;
