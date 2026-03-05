(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

type t_int = <[int]>;;
[%%expect {|
type t_int = <[int]>
|}];;

type t_char = <[char]>;;
[%%expect {|
type t_char = <[char]>
|}];;

type t_string = <[string]>;;
[%%expect {|
type t_string = <[string]>
|}];;

type t_bytes = <[bytes]>;;
[%%expect {|
type t_bytes = <[bytes]>
|}];;

type t_float = <[float]>;;
[%%expect {|
type t_float = <[float]>
|}];;

type t_float32 = <[float32]>;;
[%%expect {|
type t_float32 = <[float32]>
|}];;

type t_bool = <[bool]>;;
[%%expect {|
type t_bool = <[bool]>
|}];;

type t_unit = <[unit]>;;
[%%expect {|
type t_unit = <[unit]>
|}];;

type t_exn = <[exn]>;;
[%%expect {|
type t_exn = <[exn]>
|}];;

type t_array = <[int array]>;;
[%%expect {|
type t_array = <[int array]>
|}];;

type t_iarray = <[int iarray]>;;
[%%expect {|
type t_iarray = <[int iarray]>
|}];;

type t_list = <[int list]>;;
[%%expect {|
type t_list = <[int list]>
|}];;

type t_option = <[int option]>;;
[%%expect {|
type t_option = <[int option]>
|}];;

type t_nativeint = <[nativeint]>;;
[%%expect {|
type t_nativeint = <[nativeint]>
|}];;

type t_int32 = <[int32]>;;
[%%expect {|
type t_int32 = <[int32]>
|}];;

type t_int64 = <[int64]>;;
[%%expect {|
type t_int64 = <[int64]>
|}];;

type t_lazy_t = <[int lazy_t]>;;
[%%expect {|
type t_lazy_t = <[int lazy_t]>
|}];;

type t_extension_constructor = <[extension_constructor]>;;
[%%expect {|
type t_extension_constructor = <[extension_constructor]>
|}];;

type t_floatarray = <[floatarray]>;;
[%%expect {|
type t_floatarray = <[floatarray]>
|}];;

type t_lexing_position = <[lexing_position]>;;
[%%expect {|
type t_lexing_position = <[lexing_position]>
|}];;

type t_expr = <[<[int]> expr]>;;
[%%expect {|
type t_expr = <[<[int]> expr]>
|}];;

type t_unboxed_float = <[float#]>;;
[%%expect {|
type t_unboxed_float = <[float#]>
|}];;

type t_unboxed_nativeint = <[nativeint#]>;;
[%%expect {|
type t_unboxed_nativeint = <[nativeint#]>
|}];;

type t_unboxed_int32 = <[int32#]>;;
[%%expect {|
type t_unboxed_int32 = <[int32#]>
|}];;

type t_unboxed_int64 = <[int64#]>;;
[%%expect {|
type t_unboxed_int64 = <[int64#]>
|}];;

type t_int8x16 = <[int8x16]>;;
[%%expect {|
type t_int8x16 = <[int8x16]>
|}];;

type t_int16x8 = <[int16x8]>;;
[%%expect {|
type t_int16x8 = <[int16x8]>
|}];;

type t_int32x4 = <[int32x4]>;;
[%%expect {|
type t_int32x4 = <[int32x4]>
|}];;

type t_int64x2 = <[int64x2]>;;
[%%expect {|
type t_int64x2 = <[int64x2]>
|}];;

type t_float32x4 = <[float32x4]>;;
[%%expect {|
type t_float32x4 = <[float32x4]>
|}];;

type t_float64x2 = <[float64x2]>;;
[%%expect {|
type t_float64x2 = <[float64x2]>
|}];;

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
>> Fatal error: estimate_type_jkind: Non-quote-kinded splice type
Uncaught exception: Misc.Fatal_error

|}];;

type ('a, 'b, 'c) s4 = <[$'a list -> $'b option -> $'c]> expr;;
[%%expect {|
>> Fatal error: estimate_type_jkind: Non-quote-kinded splice type
Uncaught exception: Misc.Fatal_error

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
       as encountered at line 1, characters 10-23.
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
       as encountered at line 1, characters 13-16.
       Did you forget to insert a quotation?
|}];;

type 'a t4 = $'a -> $'a;;
[%%expect {|
Line 1, characters 13-16:
1 | type 'a t4 = $'a -> $'a;;
                 ^^^
Error: Splices ($) are not allowed in the initial stage,
       as encountered at line 1, characters 13-16.
       Did you forget to insert a quotation?
|}];;

let p x = <[x]>;;
[%%expect {|
Line 1, characters 12-13:
1 | let p x = <[x]>;;
                ^
Error: Identifier "x" is used at line 1, characters 12-13,
       inside a quotation (<[ ... ]>);
       it is introduced at line 1, characters 6-7, outside any quotations.
|}];;

let f (x : $'a) = x
[%%expect {|
Line 1, characters 11-14:
1 | let f (x : $'a) = x
               ^^^
Error: Splices ($) are not allowed in the initial stage,
       as encountered at line 1, characters 11-14.
       Did you forget to insert a quotation?
|}];;

let foo1 (x: 'a) = <[fun (y : $'a) -> 1]>;;
[%%expect {|
Line 1, characters 25-34:
1 | let foo1 (x: 'a) = <[fun (y : $'a) -> 1]>;;
                             ^^^^^^^^^
Error: This pattern matches values of type "$('a)"
       but a pattern was expected which matches values of type
         "('b : '_representable_layout_1)"
       The layout of $('a) is any
         because it's assigned a dummy kind that should have been overwritten.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of $('a) must be representable
         because we must know concretely how to pass a function argument.
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
val foo4 :
  ('a : <[value_or_null]>) ('b : <[value_or_null]>).
    'a expr -> <[$('b) -> $('a) * $('b)]> expr =
  <fun>
|}];;

let foo5 (x: <['a]> expr) = <[fun (y : 'a) -> ($x, y)]>;;
[%%expect {|
val foo5 :
  ('a : <[value_or_null]>). 'a expr -> <[$('a) -> $('a) * $('a)]> expr =
  <fun>
|}];;

let foo6 (type a) (type b) x = <[fun (y : a) -> y]>;;
[%%expect {|
Line 1, characters 42-43:
1 | let foo6 (type a) (type b) x = <[fun (y : a) -> y]>;;
                                              ^
Error: Identifier "a" is used at line 1, characters 42-43,
       inside a quotation (<[ ... ]>);
       it is introduced at line 1, characters 15-16, outside any quotations.
|}];;

let foo7 (type a) (type b) x = <[fun (y : $a) -> y]>;;
[%%expect {|
>> Fatal error: estimate_type_jkind: Non-quote-kinded splice type
Uncaught exception: Misc.Fatal_error

|}];;

let foo7' = (fun (type a) (type b) x -> <[fun (y : $a) -> y]>) 42;;
[%%expect {|
>> Fatal error: estimate_type_jkind: Non-quote-kinded splice type
Uncaught exception: Misc.Fatal_error

|}];;

let foo8 (type a) (type b) x = <[fun ((p, q) : $a * $b) -> ($x, (p, q))]> <["foo"]>;;
[%%expect {|
>> Fatal error: estimate_type_jkind: Non-quote-kinded splice type
Uncaught exception: Misc.Fatal_error

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
Error: Constructor "A" used at line 3, characters 2-3
       cannot be used in this context;
       "A" is not defined inside a quotation (<[ ... ]>).
Hint: Constructor "A" is defined outside any quotations.
|}];;

type t5 = int;;

type s = <[t5#]>;;
[%%expect {|
type t5 = int
Line 3, characters 11-14:
3 | type s = <[t5#]>;;
               ^^^
Error: Identifier "t5" is used at line 3, characters 11-14,
       inside a quotation (<[ ... ]>);
       it is introduced at line 1, characters 0-13, outside any quotations.
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
Line 1, characters 8-28:
1 | let bar (f : <[int -> int]>) = f 42;;
            ^^^^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type "<[int -> int]>"
       but a pattern was expected which matches values of type
         "('a : value_or_null)"
       The kind of <[int -> int]> is
           <[value mod aliased immutable non_float]>
         because it's a function type.
       But the kind of <[int -> int]> must be a subkind of value_or_null
         because we must know concretely how to pass a function argument.
|}];;

(* The mk_pair examples exist to test whether unification behaves well when
   splices and quotations are present. *)

let mk_pair x = <[$x, $x]>;;
[%%expect {|
val mk_pair : ('a : <[value_or_null]>). 'a expr -> <[$('a) * $('a)]> expr =
  <fun>
|}];;

mk_pair <[123]>;;
[%%expect {|
Line 1, characters 8-15:
1 | mk_pair <[123]>;;
            ^^^^^^^
Error: This expression has type "<['a]> expr"
       but an expression was expected of type "'b expr"
       Type "<['a]>" is not compatible with type "'b"
       The layout of <['a]> is any
         because it's the type of an expression inside of a quote.
       But the layout of <['a]> must be a sublayout of value
         because of the definition of mk_pair at line 1, characters 12-26.
|}];;

mk_pair <[[]]>;;
[%%expect {|
Line 1, characters 8-14:
1 | mk_pair <[[]]>;;
            ^^^^^^
Error: This expression has type "<['a]> expr"
       but an expression was expected of type "'b expr"
       Type "<['a]>" is not compatible with type "'b"
       The layout of <['a]> is any
         because it's the type of an expression inside of a quote.
       But the layout of <['a]> must be a sublayout of value
         because of the definition of mk_pair at line 1, characters 12-26.
|}];;

mk_pair <[None]>;;
[%%expect {|
Line 1, characters 8-16:
1 | mk_pair <[None]>;;
            ^^^^^^^^
Error: This expression has type "<['a]> expr"
       but an expression was expected of type "'b expr"
       Type "<['a]>" is not compatible with type "'b"
       The layout of <['a]> is any
         because it's the type of an expression inside of a quote.
       But the layout of <['a]> must be a sublayout of value
         because of the definition of mk_pair at line 1, characters 12-26.
|}];;

mk_pair <[Some 123]>;;
[%%expect {|
Line 1, characters 8-20:
1 | mk_pair <[Some 123]>;;
            ^^^^^^^^^^^^
Error: This expression has type "<['a]> expr"
       but an expression was expected of type "'b expr"
       Type "<['a]>" is not compatible with type "'b"
       The layout of <['a]> is any
         because it's the type of an expression inside of a quote.
       But the layout of <['a]> must be a sublayout of value
         because of the definition of mk_pair at line 1, characters 12-26.
|}];;

mk_pair <[fun () -> 42]>;;
[%%expect {|
Line 1, characters 8-24:
1 | mk_pair <[fun () -> 42]>;;
            ^^^^^^^^^^^^^^^^
Error: This expression has type "<['a]> expr"
       but an expression was expected of type "'b expr"
       Type "<['a]>" is not compatible with type "'b"
       The layout of <['a]> is any
         because it's the type of an expression inside of a quote.
       But the layout of <['a]> must be a sublayout of value
         because of the definition of mk_pair at line 1, characters 12-26.
|}];;

mk_pair <[fun x -> x]>;;
[%%expect {|
Line 1, characters 8-22:
1 | mk_pair <[fun x -> x]>;;
            ^^^^^^^^^^^^^^
Error: This expression has type "<['a]> expr"
       but an expression was expected of type "'b expr"
       Type "<['a]>" is not compatible with type "'b"
       The layout of <['a]> is any
         because it's the type of an expression inside of a quote.
       But the layout of <['a]> must be a sublayout of value
         because of the definition of mk_pair at line 1, characters 12-26.
|}];;

(* Type algebra checks. *)

fun (x: 'a) -> (x: <[<[<[$($($'a))]>]>]>);;
[%%expect {|
>> Fatal error: estimate_type_jkind: Non-quote-kinded splice type
Uncaught exception: Misc.Fatal_error

|}];;

fun (x: <[<[<[$($($'a))]>]>]>) -> (x: 'a);;
[%%expect {|
Line 1, characters 4-30:
1 | fun (x: <[<[<[$($($'a))]>]>]>) -> (x: 'a);;
        ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type "'a" = "('a : any)"
       but a pattern was expected which matches values of type
         "('b : '_representable_layout_2)"
       The layout of 'a is any
         because it's a fresh unification variable.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of 'a must be representable
         because we must know concretely how to pass a function argument.
|}];;

fun (x: <[<[<[$($'a)]>]>]>) -> (x: 'a);;
[%%expect {|
Line 1, characters 4-27:
1 | fun (x: <[<[<[$($'a)]>]>]>) -> (x: 'a);;
        ^^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type "<['a]>"
       but a pattern was expected which matches values of type
         "('b : '_representable_layout_3)"
       The layout of <['a]> is any
         because it's a fresh unification variable.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of <['a]> must be representable
         because we must know concretely how to pass a function argument.
|}];;

(* Eta expansion of quotes/splices *)

let eta (type a) (x : a expr) : a expr = <[ $x ]>
[%%expect {|
>> Fatal error: estimate_type_jkind: Non-quote-kinded splice type
Uncaught exception: Misc.Fatal_error

|}]

let eta1 (type a) = <[ fun (x : $a expr) : $a expr -> <[ $x ]> ]>
[%%expect {|
>> Fatal error: estimate_type_jkind: Non-quote-kinded splice type
Uncaught exception: Misc.Fatal_error

|}]

let eta1' = <[ fun (type a) (x : a) : a -> $(<[ x ]>) ]>
[%%expect {|
val eta1' : ('a : <[value]>). <[$('a) -> $('a)]> expr =
  <[fun (type a) (x : a) -> (x : a)]>
|}]

(* Applicative *)

let app (type a b) (f : <[$a -> $b]> expr) (x : a expr) =
  <[ $f $x ]>
[%%expect {|
>> Fatal error: estimate_type_jkind: Non-quote-kinded splice type
Uncaught exception: Misc.Fatal_error

|}]

let both (type a b) (p : a expr * b expr) : <[$a * $b]> expr =
  let (x, y) = p in <[ $x, $y ]>
[%%expect {|
>> Fatal error: estimate_type_jkind: Non-quote-kinded splice type
Uncaught exception: Misc.Fatal_error

|}]

(* Type equality *)

module M : sig
  type 'a t = <[$('a) -> $('a)]> expr
end = struct
  type 'a t = <[$('a) -> $('a)]> expr
end
[%%expect{|
module M : sig type 'a t = <[$('a) -> $('a)]> expr end
|}]

module M : sig
  type t = <[int]> expr
end = struct
  type t = <[int]> expr
end
[%%expect{|
module M : sig type t = <[int]> expr end
|}]

module M : sig
  type t = <[int]> expr
end = struct
  type t = <[string]> expr
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = <[string]> expr
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = <[string]> expr end
       is not included in
         sig type t = <[int]> expr end
       Type declarations do not match:
         type t = <[string]> expr
       is not included in
         type t = <[int]> expr
       The type "<[string]> expr" is not equal to the type "<[int]> expr"
       Type "string" is not equal to type "int"
|}]

module M : sig
  type t = <[<[int]> expr -> <[int]> expr]> expr
end = struct
  type t = <[<[int]> expr -> <[int]> expr]> expr
end
[%%expect{|
module M : sig type t = <[<[int]> expr -> <[int]> expr]> expr end
|}]
