(* TEST
 expect;
*)

(* These tests show how potential ambiguities are resolved
   between the types #c and int64_u.
*)

(* Basic syntax: int64_u is an unboxed int64. *)
type t = int64_u;;
let f (_ : int64_u) = ();;
[%%expect {|
type t = int64_u
val f : int64_u -> unit = <fun>
|}];;

type t = C of int64_u;;
[%%expect {|
type t = C of int64_u
|}];;

type t = C : int64_u -> t;;
[%%expect {|
type t = C : int64_u -> t
|}];;

(* int64_u works as an argument to normal type constructors, not just
   classes, even though many of the rest of the tests in this file are concerned
   with classes.
*)
type t = int64_u list;;
[%%expect {|
Line 1, characters 9-16:
1 | type t = int64_u list;;
             ^^^^^^^
Error: This type "int64_u" should be an instance of type "('a : value_or_null)"
       The layout of int64_u is bits64
         because it is the primitive type int64_u.
       But the layout of int64_u must be a value layout
         because the type argument of list has layout value_or_null.
|}];;

let f (_ : int64_u list) = ();;
[%%expect {|
Line 1, characters 11-18:
1 | let f (_ : int64_u list) = ();;
               ^^^^^^^
Error: This type "int64_u" should be an instance of type "('a : value_or_null)"
       The layout of int64_u is bits64
         because it is the primitive type int64_u.
       But the layout of int64_u must be a value layout
         because the type argument of list has layout value_or_null.
|}];;

type t = C of int64_u list;;
[%%expect {|
Line 1, characters 14-21:
1 | type t = C of int64_u list;;
                  ^^^^^^^
Error: This type "int64_u" should be an instance of type "('a : value_or_null)"
       The layout of int64_u is bits64
         because it is the primitive type int64_u.
       But the layout of int64_u must be a value layout
         because the type argument of list has layout value_or_null.
|}];;

type t = C : int64_u list -> t;;
[%%expect {|
Line 1, characters 13-20:
1 | type t = C : int64_u list -> t;;
                 ^^^^^^^
Error: This type "int64_u" should be an instance of type "('a : value_or_null)"
       The layout of int64_u is bits64
         because it is the primitive type int64_u.
       But the layout of int64_u must be a value layout
         because the type argument of list has layout value_or_null.
|}];;

(* Syntax: int64_uc
   Interpreted as type application of [c] to [int64_u].
*)
class ['a] c = object(self)
  method x :'a = assert false
end;;
[%%expect {|
class ['a] c : object method x : 'a end
|}];;

type t = int64_uc;;
[%%expect {|
Line 1, characters 9-17:
1 | type t = int64_uc;;
             ^^^^^^^^
Error: Unbound type constructor "int64_uc"
Hint: Did you mean "int64_u"?
|}];;

let f (_ : int64_uc) = ();;
[%%expect {|
Line 1, characters 11-19:
1 | let f (_ : int64_uc) = ();;
               ^^^^^^^^
Error: Unbound type constructor "int64_uc"
Hint: Did you mean "int64_u"?
|}];;

type t = C of int64_uc;;
[%%expect {|
Line 1, characters 14-22:
1 | type t = C of int64_uc;;
                  ^^^^^^^^
Error: Unbound type constructor "int64_uc"
Hint: Did you mean "int64_u"?
|}];;

type t = C : int64_uc -> t;;
[%%expect {|
Line 1, characters 13-21:
1 | type t = C : int64_uc -> t;;
                 ^^^^^^^^
Error: Unbound type constructor "int64_uc"
Hint: Did you mean "int64_u"?
|}];;

(* Syntax: int64_u c
   Interpreted as type application of [c] to [int64_u].
*)
type t = int64_u c;;
[%%expect {|
Line 1, characters 9-16:
1 | type t = int64_u c;;
             ^^^^^^^
Error: This type "int64_u" should be an instance of type "('a : value)"
       The layout of int64_u is bits64
         because it is the primitive type int64_u.
       But the layout of int64_u must be a value layout
         because it's a type argument to a class constructor.
|}];;

let f (_ : int64_u c) = ();;
[%%expect {|
Line 1, characters 11-18:
1 | let f (_ : int64_u c) = ();;
               ^^^^^^^
Error: This type "int64_u" should be an instance of type "('a : value)"
       The layout of int64_u is bits64
         because it is the primitive type int64_u.
       But the layout of int64_u must be a value layout
         because it's a type argument to a class constructor.
|}];;

type t = C of int64_u c;;
[%%expect {|
Line 1, characters 14-21:
1 | type t = C of int64_u c;;
                  ^^^^^^^
Error: This type "int64_u" should be an instance of type "('a : value)"
       The layout of int64_u is bits64
         because it is the primitive type int64_u.
       But the layout of int64_u must be a value layout
         because it's a type argument to a class constructor.
|}];;

type t = C : int64_u c -> t;;
[%%expect {|
Line 1, characters 13-20:
1 | type t = C : int64_u c -> t;;
                 ^^^^^^^
Error: This type "int64_u" should be an instance of type "('a : value)"
       The layout of int64_u is bits64
         because it is the primitive type int64_u.
       But the layout of int64_u must be a value layout
         because it's a type argument to a class constructor.
|}];;

(* Syntax: int64 #c
   Interpreted as type application of [#c] to [int64].

   Note that [int64 #c] implicitly binds a type variable,
   so we need to name it with [as] to get some examples to
   typecheck.
*)
type t = int64 #c;;
[%%expect {|
Line 1, characters 0-17:
1 | type t = int64 #c;;
    ^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In type "int64 #c as 'a" the variable "'a" is unbound
|}];;
type t = C of int64 #c;;
[%%expect {|
Line 1, characters 0-22:
1 | type t = C of int64 #c;;
    ^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In case "C of (int64 #c as 'a)" the variable "'a" is unbound
|}];;
type 'a t = (int64 #c as 'a);;
let f (_ : int64 #c) = ();;
type 'a t = C of (int64 #c as 'a);;
type t = C : int64 #c -> t;;
[%%expect {|
type 'a t = 'a constraint 'a = int64 #c
val f : int64 #c -> unit = <fun>
type 'a t = C of 'a constraint 'a = int64 #c
type t = C : int64 #c -> t
|}];;

(* Syntax: int64 # c
   Interpreted as type application of [#c] to [int64].

   Note that [int64 #c] implicitly binds a type variable,
   so we need to name it with [as] to get some examples to
   typecheck.
*)

type t = int64 # c;;
[%%expect {|
Line 1, characters 0-18:
1 | type t = int64 # c;;
    ^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In type "int64 #c as 'a" the variable "'a" is unbound
|}];;
type t = C of int64 # c;;
[%%expect {|
Line 1, characters 0-23:
1 | type t = C of int64 # c;;
    ^^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In case "C of (int64 #c as 'a)" the variable "'a" is unbound
|}];;

type 'a t = (int64 # c as 'a);;
let f (_ : int64 # c) = ();;
type 'a t = C of (int64 # c as 'a);;
type t = C : int64 # c -> t;;
[%%expect {|
type 'a t = 'a constraint 'a = int64 #c
val f : int64 #c -> unit = <fun>
type 'a t = C of 'a constraint 'a = int64 #c
type t = C : int64 #c -> t
|}];;

(***************************)
(* Type application: it's a type error, not a parse error. *)

type t = int int64_u;;
[%%expect {|
Line 1, characters 9-20:
1 | type t = int int64_u;;
             ^^^^^^^^^^^
Error: The type constructor "int64_u" expects 0 argument(s),
       but is here applied to 1 argument(s)
|}];;

type t = (int, int) int64_u;;
[%%expect {|
Line 1, characters 9-27:
1 | type t = (int, int) int64_u;;
             ^^^^^^^^^^^^^^^^^^
Error: The type constructor "int64_u" expects 0 argument(s),
       but is here applied to 2 argument(s)
|}];;
