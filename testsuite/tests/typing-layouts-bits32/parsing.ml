(* TEST
 expect;
*)

(* These tests show how potential ambiguities are resolved
   between the types #c and int32_u.
*)

(* Basic syntax: int32_u is an unboxed int32. *)
type t = int32_u;;
let f (_ : int32_u) = ();;
[%%expect {|
type t = int32_u
val f : int32_u -> unit = <fun>
|}];;

type t = C of int32_u;;
[%%expect {|
type t = C of int32_u
|}];;

type t = C : int32_u -> t;;
[%%expect {|
type t = C : int32_u -> t
|}];;

(* int32_u works as an argument to normal type constructors, not just
   classes, even though many of the rest of the tests in this file are concerned
   with classes.
*)
type t = int32_u list;;
[%%expect {|
Line 1, characters 9-16:
1 | type t = int32_u list;;
             ^^^^^^^
Error: This type "int32_u" should be an instance of type "('a : value_or_null)"
       The layout of int32_u is bits32
         because it is the primitive type int32_u.
       But the layout of int32_u must be a value layout
         because the type argument of list has layout value_or_null.
|}];;

let f (_ : int32_u list) = ();;
[%%expect {|
Line 1, characters 11-18:
1 | let f (_ : int32_u list) = ();;
               ^^^^^^^
Error: This type "int32_u" should be an instance of type "('a : value_or_null)"
       The layout of int32_u is bits32
         because it is the primitive type int32_u.
       But the layout of int32_u must be a value layout
         because the type argument of list has layout value_or_null.
|}];;

type t = C of int32_u list;;
[%%expect {|
Line 1, characters 14-21:
1 | type t = C of int32_u list;;
                  ^^^^^^^
Error: This type "int32_u" should be an instance of type "('a : value_or_null)"
       The layout of int32_u is bits32
         because it is the primitive type int32_u.
       But the layout of int32_u must be a value layout
         because the type argument of list has layout value_or_null.
|}];;

type t = C : int32_u list -> t;;
[%%expect {|
Line 1, characters 13-20:
1 | type t = C : int32_u list -> t;;
                 ^^^^^^^
Error: This type "int32_u" should be an instance of type "('a : value_or_null)"
       The layout of int32_u is bits32
         because it is the primitive type int32_u.
       But the layout of int32_u must be a value layout
         because the type argument of list has layout value_or_null.
|}];;

(* Syntax: int32_uc
   Interpreted as type application of [c] to [int32_u].
*)
class ['a] c = object(self)
  method x :'a = assert false
end;;
[%%expect {|
class ['a] c : object method x : 'a end
|}];;

type t = int32_uc;;
[%%expect {|
Line 1, characters 9-17:
1 | type t = int32_uc;;
             ^^^^^^^^
Error: Unbound type constructor "int32_uc"
Hint: Did you mean "int32_u"?
|}];;

let f (_ : int32_uc) = ();;
[%%expect {|
Line 1, characters 11-19:
1 | let f (_ : int32_uc) = ();;
               ^^^^^^^^
Error: Unbound type constructor "int32_uc"
Hint: Did you mean "int32_u"?
|}];;

type t = C of int32_uc;;
[%%expect {|
Line 1, characters 14-22:
1 | type t = C of int32_uc;;
                  ^^^^^^^^
Error: Unbound type constructor "int32_uc"
Hint: Did you mean "int32_u"?
|}];;

type t = C : int32_uc -> t;;
[%%expect {|
Line 1, characters 13-21:
1 | type t = C : int32_uc -> t;;
                 ^^^^^^^^
Error: Unbound type constructor "int32_uc"
Hint: Did you mean "int32_u"?
|}];;

(* Syntax: int32_u c
   Interpreted as type application of [c] to [int32_u].
*)
type t = int32_u c;;
[%%expect {|
Line 1, characters 9-16:
1 | type t = int32_u c;;
             ^^^^^^^
Error: This type "int32_u" should be an instance of type "('a : value)"
       The layout of int32_u is bits32
         because it is the primitive type int32_u.
       But the layout of int32_u must be a value layout
         because it's a type argument to a class constructor.
|}];;

let f (_ : int32_u c) = ();;
[%%expect {|
Line 1, characters 11-18:
1 | let f (_ : int32_u c) = ();;
               ^^^^^^^
Error: This type "int32_u" should be an instance of type "('a : value)"
       The layout of int32_u is bits32
         because it is the primitive type int32_u.
       But the layout of int32_u must be a value layout
         because it's a type argument to a class constructor.
|}];;

type t = C of int32_u c;;
[%%expect {|
Line 1, characters 14-21:
1 | type t = C of int32_u c;;
                  ^^^^^^^
Error: This type "int32_u" should be an instance of type "('a : value)"
       The layout of int32_u is bits32
         because it is the primitive type int32_u.
       But the layout of int32_u must be a value layout
         because it's a type argument to a class constructor.
|}];;

type t = C : int32_u c -> t;;
[%%expect {|
Line 1, characters 13-20:
1 | type t = C : int32_u c -> t;;
                 ^^^^^^^
Error: This type "int32_u" should be an instance of type "('a : value)"
       The layout of int32_u is bits32
         because it is the primitive type int32_u.
       But the layout of int32_u must be a value layout
         because it's a type argument to a class constructor.
|}];;

(* Syntax: int32 #c
   Interpreted as type application of [#c] to [int32].

   Note that [int32 #c] implicitly binds a type variable,
   so we need to name it with [as] to get some examples to
   typecheck.
*)
type t = int32 #c;;
[%%expect {|
Line 1, characters 0-17:
1 | type t = int32 #c;;
    ^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In type "int32 #c as 'a" the variable "'a" is unbound
|}];;
type t = C of int32 #c;;
[%%expect {|
Line 1, characters 0-22:
1 | type t = C of int32 #c;;
    ^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In case "C of (int32 #c as 'a)" the variable "'a" is unbound
|}];;
type 'a t = (int32 #c as 'a);;
let f (_ : int32 #c) = ();;
type 'a t = C of (int32 #c as 'a);;
type t = C : int32 #c -> t;;
[%%expect {|
type 'a t = 'a constraint 'a = int32 #c
val f : int32 #c -> unit = <fun>
type 'a t = C of 'a constraint 'a = int32 #c
type t = C : int32 #c -> t
|}];;

(* Syntax: int32 # c
   Interpreted as type application of [#c] to [int32].

   Note that [int32 #c] implicitly binds a type variable,
   so we need to name it with [as] to get some examples to
   typecheck.
*)

type t = int32 # c;;
[%%expect {|
Line 1, characters 0-18:
1 | type t = int32 # c;;
    ^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In type "int32 #c as 'a" the variable "'a" is unbound
|}];;
type t = C of int32 # c;;
[%%expect {|
Line 1, characters 0-23:
1 | type t = C of int32 # c;;
    ^^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In case "C of (int32 #c as 'a)" the variable "'a" is unbound
|}];;

type 'a t = (int32 # c as 'a);;
let f (_ : int32 # c) = ();;
type 'a t = C of (int32 # c as 'a);;
type t = C : int32 # c -> t;;
[%%expect {|
type 'a t = 'a constraint 'a = int32 #c
val f : int32 #c -> unit = <fun>
type 'a t = C of 'a constraint 'a = int32 #c
type t = C : int32 #c -> t
|}];;

(***************************)
(* Type application: it's a type error, not a parse error. *)

type t = int int32_u;;
[%%expect {|
Line 1, characters 9-20:
1 | type t = int int32_u;;
             ^^^^^^^^^^^
Error: The type constructor "int32_u" expects 0 argument(s),
       but is here applied to 1 argument(s)
|}];;

type t = (int, int) int32_u;;
[%%expect {|
Line 1, characters 9-27:
1 | type t = (int, int) int32_u;;
             ^^^^^^^^^^^^^^^^^^
Error: The type constructor "int32_u" expects 0 argument(s),
       but is here applied to 2 argument(s)
|}];;
