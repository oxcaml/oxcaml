(* TEST
 flags = "-extension runtime_metaprogramming";
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
Line 1, characters 35-37:
1 | type ('a, 'b) s5 = <[$'a -> [`A of 'b]]> expr;;
                                       ^^
Error: Type variable "'b" is used inside a quotation (<[ ... ]>),
       it already occurs outside any quotations.
       Hint: Consider using "$'b".
|}];;

type s6 = <[string -> bool -> [`A | `B of string]]> expr;;
[%%expect {|
type s6 = <[string -> bool -> [ `A | `B of string ]]> expr
|}];;

type s7 = $(int -> int);;
[%%expect {|
Line 1, characters 10-23:
1 | type s7 = $(int -> int);;
              ^^^^^^^^^^^^^
Error: Splices ($) are not allowed in the initial stage,
       as encountered at Line 1, characters 10-23.
       Did you forget to insert a quotation?
|}];;

type 'a t1 = 'a expr;;
[%%expect {|
type 'a t1 = 'a expr
|}];;

type 'a t2 = <['a]> expr;;
[%%expect {|
Line 1, characters 15-17:
1 | type 'a t2 = <['a]> expr;;
                   ^^
Error: Type variable "'a" is used inside a quotation (<[ ... ]>),
       it already occurs outside any quotations.
       Hint: Consider using "$'a".
|}];;

type 'a t3 = $'a -> $'a -> 'a expr;;
[%%expect {|
Line 1, characters 13-16:
1 | type 'a t3 = $'a -> $'a -> 'a expr;;
                 ^^^
Error: Splices ($) are not allowed in the initial stage,
       as encountered at Line 1, characters 13-16.
       Did you forget to insert a quotation?
|}];;

type 'a t4 = $'a -> $'a;;
[%%expect {|
Line 1, characters 13-16:
1 | type 'a t4 = $'a -> $'a;;
                 ^^^
Error: Splices ($) are not allowed in the initial stage,
       as encountered at Line 1, characters 13-16.
       Did you forget to insert a quotation?
|}];;

let p x = <[x]>;;
[%%expect {|
Line 1, characters 12-13:
1 | let p x = <[x]>;;
                ^
Error: Identifier "x" is used at Line 1, characters 12-13,
       inside a quotation (<[ ... ]>);
       it is introduced at Line 1, characters 6-7, outside any quotations.
|}];;

let f (x : $'a) = x
[%%expect {|
Line 1, characters 11-14:
1 | let f (x : $'a) = x
               ^^^
Error: Splices ($) are not allowed in the initial stage,
       as encountered at Line 1, characters 11-14.
       Did you forget to insert a quotation?
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
Error: Type variable "'a" is used inside a quotation (<[ ... ]>),
       it already occurs outside any quotations.
       Hint: Consider using "$'a".
|}];;

let foo3 (x: 'a) = <[fun (y : <['a]>) -> 1]>;;
[%%expect {|
Line 1, characters 32-34:
1 | let foo3 (x: 'a) = <[fun (y : <['a]>) -> 1]>;;
                                    ^^
Error: Type variable "'a" is used inside 2 layers of quotation (<[ ... ]>),
       it already occurs outside any quotations.
       Hint: Consider using "$($'a)".
|}];;

let foo4 (x: <['a]> expr) = <[fun (y : 'b) -> ($x, y)]>;;
[%%expect {|
val foo4 : 'a expr -> <[$('b) -> $('a) * $('b)]> expr = <fun>
|}];;

let foo5 (x: <['a]> expr) = <[fun (y : 'a) -> ($x, y)]>;;
[%%expect {|
val foo5 : 'a expr -> <[$('a) -> $('a) * $('a)]> expr = <fun>
|}];;

let foo6 (type a) (type b) x = <[fun (y : a) -> y]>;;
[%%expect {|
Line 1, characters 42-43:
1 | let foo6 (type a) (type b) x = <[fun (y : a) -> y]>;;
                                              ^
Error: Identifier "a" is used at Line 1, characters 42-43,
       inside a quotation (<[ ... ]>);
       it is introduced at Line 1, characters 15-16, outside any quotations.
|}];;

let foo7 (type a) (type b) x = <[fun (y : $a) -> y]>;;
[%%expect {|
val foo7 : 'b -> <[$('a) -> $('a)]> expr = <fun>
|}];;

let foo7' = (fun (type a) (type b) x -> <[fun (y : $a) -> y]>) 42;;
[%%expect {|
val foo7' : <[$('_a) -> $('_a)]> expr = <[fun (y : _) -> y]>
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
Error: Type variable "'a" is used outside any quotations,
       it already occurs inside a quotation (<[ ... ]>).
       Hint: Consider using "<['a]>".
|}];;

<[fun (type a) (type b) (x : a) (y : b) -> (x, y)]>;;
[%%expect {|
- : <[$('a) -> $('b) -> $('a) * $('b)]> expr =
<[fun (type a) (type b) (x : a) (y : b) -> (x, y)]>
|}];;

type t4 = A | B;;

<[A]>;;
[%%expect {|
type t4 = A | B
Line 3, characters 2-3:
3 | <[A]>;;
      ^
Error: Constructor "A" used at Line 3, characters 2-3
       cannot be used in this context;
       "A" is not defined inside a quotation (<[ ... ]>).
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

let bar (f : <[int -> int]>) = f 42;;
[%%expect {|
Line 1, characters 31-32:
1 | let bar (f : <[int -> int]>) = f 42;;
                                   ^
Error: This expression has type "<[int -> int]>"
       This is not a function; it cannot be applied.
|}];;

(* The mk_pair examples exist to test whether unification behaves well when
   splices and quotations are present. *)

let mk_pair x = <[$x, $x]>;;
[%%expect {|
val mk_pair : 'a expr -> <[$('a) * $('a)]> expr = <fun>
|}];;

mk_pair <[123]>;;
[%%expect {|
- : <[int * int]> expr = <[(123, 123)]>
|}];;

mk_pair <[[]]>;;
[%%expect {|
- : <[$('a) list * $('a) list]> expr = <[([], [])]>
|}];;

mk_pair <[None]>;;
[%%expect {|
- : <[$('a) option * $('a) option]> expr = <[(None, None)]>
|}];;

mk_pair <[Some 123]>;;
[%%expect {|
- : <[int option * int option]> expr = <[((Some 123), (Some 123))]>
|}];;

mk_pair <[fun () -> 42]>;;
[%%expect {|
- : <[unit -> int * unit -> int]> expr = <[((fun () -> 42), (fun () -> 42))]>
|}];;

mk_pair <[fun x -> x]>;;
[%%expect {|
- : <['_weak1 -> '_weak1 * '_weak1 -> '_weak1]> expr =
<[((fun x -> x), (fun x -> x))]>
|}];;

(* Type algebra checks. *)

fun (x: 'a) -> (x: <[<[<[$($($'a))]>]>]>);;
[%%expect {|
- : 'a -> 'a = <fun>
|}];;

fun (x: <[<[<[$($($'a))]>]>]>) -> (x: 'a);;
[%%expect {|
- : 'a -> 'a = <fun>
|}];;

fun (x: <[<[<[$($'a)]>]>]>) -> (x: 'a);;
[%%expect {|
Line 1, characters 35-37:
1 | fun (x: <[<[<[$($'a)]>]>]>) -> (x: 'a);;
                                       ^^
Error: Type variable "'a" is used outside any quotations,
       it already occurs inside a quotation (<[ ... ]>).
       Hint: Consider using "<['a]>".
|}];;

let eta (type a) (x : a expr) : a expr = <[ $x ]>
[%%expect {|
val eta : 'a expr -> 'a expr = <fun>
|}]

(* Inclusion checks *)

(*  [M1] and [M2] pass solely due to stage normalisation
    (moving all type variables to occur at stage offset zero) *)

module M1 : sig
  val foo : 'a -> <[$('a) -> int]> expr
end = struct
  let foo (x: 'a) = <[fun (y : $'a) -> 1]>;;
end

[%%expect{|
module M1 : sig val foo : 'a -> <[$('a) -> int]> expr end
|}]

module M1' : sig
  val foo : <['a]> expr -> <['a -> int]> expr
end = struct
  let foo (x: 'a expr) = <[fun (y : $'a) -> 1]>;;
end

[%%expect{|
module M1' : sig val foo : 'a expr -> <[$('a) -> int]> expr end
|}]

module M1'' : sig
  val foo : 'a expr -> <[$('a) -> int]> expr
end = struct
  let foo (x: <['a]> expr) = <[fun (y : 'a) -> 1]>;;
end

[%%expect{|
module M1'' : sig val foo : 'a expr -> <[$('a) -> int]> expr end
|}]

(*  Simple functions with a type variable under a quote-splice *)
module M2 = struct
  let f (x : <[ 'a ]> expr) = <[ ($x, $x) ]>
end

[%%expect{|
module M2 : sig val f : 'a expr -> <[$('a) * $('a)]> expr end
|}]

(*  Checking [M2'] does not rely on any quote-splice inverses:
    'a will be unified with <[int]> when checking the function parameter side,
    skipping any nontrivial reasoning.  *)
module M2' : sig
  val f : <[ int ]> expr -> <[ int * int ]> expr
end = M2

[%%expect{|
module M2' : sig val f : <[int]> expr -> <[int * int]> expr end
|}]

(*  [M3] is trickier, leading to the case $'a < t *)

module M3 = struct
  let f = let (x : <[ 'a -> unit ]> expr) = <[ fun _ -> () ]> in <[ ($x, $x) ]>
end

[%%expect{|
module M3 : sig val f : <[($('a) -> unit) * ($('a) -> unit)]> expr end
|}]

(*   $('a) = int  <=>  'a = <[int]> *)
module M3' : sig
  val f : <[ (int -> unit) * (int -> unit) ]> expr
end = M3

[%%expect{|
module M3' : sig val f : <[(int -> unit) * (int -> unit)]> expr end
|}]

(*   [M3''] is a simple failure to check the error message when we end up with
     contradicting quoted unificands *)
(*   string = $('a) = int  <=>  <[string]> = 'a = <[int]>  <=>  string = int (error!) *)
module M3'' : sig
  val f : <[ (string -> unit) * (int -> unit) ]> expr
end = M3

[%%expect{|
Line 3, characters 6-8:
3 | end = M3
          ^^
Error: Signature mismatch:
       Modules do not match:
         sig val f : <[($('a) -> unit) * ($('a) -> unit)]> expr end
       is not included in
         sig val f : <[(string -> unit) * (int -> unit)]> expr end
       Values do not match:
         val f : <[($('a) -> unit) * ($('a) -> unit)]> expr
       is not included in
         val f : <[(string -> unit) * (int -> unit)]> expr
       The type "<[(string -> unit) * (string -> unit)]> expr"
       is not compatible with the type
         "<[(string -> unit) * (int -> unit)]> expr"
       Type "string" = "string" is not compatible with type "int"
|}]

(*  [M4] is analogous to [M3], but featuring an uninhabited type *)

module M4 : sig
  val x : <[ 'a * 'a ]> expr
end = struct
  let x = <[ let y = Obj.magic () in (y, y) ]>
end

[%%expect{|
module M4 : sig val x : <[$('a) * $('a)]> expr end
|}]

module M4' : sig
  val x : <[ int * int ]> expr
end = M4

[%%expect{|
module M4' : sig val x : <[int * int]> expr end
|}]

(* Analogously to [M3''], [M4''] checks we detect any failing unifications *)
module M4'' : sig
  val x : <[ int * string ]> expr
end = M4

[%%expect{|
Line 3, characters 6-8:
3 | end = M4
          ^^
Error: Signature mismatch:
       Modules do not match:
         sig val x : <[$('a) * $('a)]> expr end
       is not included in
         sig val x : <[int * string]> expr end
       Values do not match:
         val x : <[$('a) * $('a)]> expr
       is not included in
         val x : <[int * string]> expr
       The type "<[int * int]> expr" is not compatible with the type
         "<[int * string]> expr"
       Type "int" = "int" is not compatible with type "string"
|}]
