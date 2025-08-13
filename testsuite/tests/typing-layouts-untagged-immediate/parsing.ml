(* TEST
 expect;
*)

(* These tests show how potential ambiguities are resolved
   between the types #c and int#.
*)

(* Basic syntax: int# is an unboxed int. *)
type t = int#;;
let f (_ : int#) = ();;
[%%expect {|
type t = int#
val f : int# -> unit = <fun>
|}];;

type t = C of int#;;
[%%expect {|
type t = C of int#
|}];;

type t = C : int# -> t;;
[%%expect {|
type t = C : int# -> t
|}];;

(* int# works as an argument to normal type constructors, not just
   classes, even though many of the rest of the tests in this file are concerned
   with classes.
*)
type t = int# list;;
[%%expect {|
Line 1, characters 9-13:
1 | type t = int# list;;
             ^^^^
Error: This type "int#" should be an instance of type "('a : value_or_null)"
       The layout of int# is untagged_immediate
         because it is the unboxed version of the primitive type int.
       But the layout of int# must be a sublayout of value
         because the type argument of list has layout value_or_null.
|}];;

let f (_ : int# list) = ();;
[%%expect {|
Line 1, characters 11-15:
1 | let f (_ : int# list) = ();;
               ^^^^
Error: This type "int#" should be an instance of type "('a : value_or_null)"
       The layout of int# is untagged_immediate
         because it is the unboxed version of the primitive type int.
       But the layout of int# must be a sublayout of value
         because the type argument of list has layout value_or_null.
|}];;

type t = C of int# list;;
[%%expect {|
Line 1, characters 14-18:
1 | type t = C of int# list;;
                  ^^^^
Error: This type "int#" should be an instance of type "('a : value_or_null)"
       The layout of int# is untagged_immediate
         because it is the unboxed version of the primitive type int.
       But the layout of int# must be a sublayout of value
         because the type argument of list has layout value_or_null.
|}];;

type t = C : int# list -> t;;
[%%expect {|
Line 1, characters 13-17:
1 | type t = C : int# list -> t;;
                 ^^^^
Error: This type "int#" should be an instance of type "('a : value_or_null)"
       The layout of int# is untagged_immediate
         because it is the unboxed version of the primitive type int.
       But the layout of int# must be a sublayout of value
         because the type argument of list has layout value_or_null.
|}];;

(* Syntax: int#c
   Interpreted as type application of [c] to [int#].
*)
class ['a] c = object(self)
  method x :'a = assert false
end;;
[%%expect {|
class ['a] c : object method x : 'a end
|}];;

type t = int#c;;
[%%expect {|
Line 1, characters 9-13:
1 | type t = int#c;;
             ^^^^
Error: This type "int#" should be an instance of type "('a : value)"
       The layout of int# is untagged_immediate
         because it is the unboxed version of the primitive type int.
       But the layout of int# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

let f (_ : int#c) = ();;
[%%expect {|
Line 1, characters 11-15:
1 | let f (_ : int#c) = ();;
               ^^^^
Error: This type "int#" should be an instance of type "('a : value)"
       The layout of int# is untagged_immediate
         because it is the unboxed version of the primitive type int.
       But the layout of int# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C of int#c;;
[%%expect {|
Line 1, characters 14-18:
1 | type t = C of int#c;;
                  ^^^^
Error: This type "int#" should be an instance of type "('a : value)"
       The layout of int# is untagged_immediate
         because it is the unboxed version of the primitive type int.
       But the layout of int# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C : int#c -> t;;
[%%expect {|
Line 1, characters 13-17:
1 | type t = C : int#c -> t;;
                 ^^^^
Error: This type "int#" should be an instance of type "('a : value)"
       The layout of int# is untagged_immediate
         because it is the unboxed version of the primitive type int.
       But the layout of int# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

(* Syntax: int# c
   Interpreted as type application of [c] to [int#].
*)
type t = int# c;;
[%%expect {|
Line 1, characters 9-13:
1 | type t = int# c;;
             ^^^^
Error: This type "int#" should be an instance of type "('a : value)"
       The layout of int# is untagged_immediate
         because it is the unboxed version of the primitive type int.
       But the layout of int# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

let f (_ : int# c) = ();;
[%%expect {|
Line 1, characters 11-15:
1 | let f (_ : int# c) = ();;
               ^^^^
Error: This type "int#" should be an instance of type "('a : value)"
       The layout of int# is untagged_immediate
         because it is the unboxed version of the primitive type int.
       But the layout of int# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C of int# c;;
[%%expect {|
Line 1, characters 14-18:
1 | type t = C of int# c;;
                  ^^^^
Error: This type "int#" should be an instance of type "('a : value)"
       The layout of int# is untagged_immediate
         because it is the unboxed version of the primitive type int.
       But the layout of int# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C : int# c -> t;;
[%%expect {|
Line 1, characters 13-17:
1 | type t = C : int# c -> t;;
                 ^^^^
Error: This type "int#" should be an instance of type "('a : value)"
       The layout of int# is untagged_immediate
         because it is the unboxed version of the primitive type int.
       But the layout of int# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

(* Syntax: int #c
   Interpreted as type application of [#c] to [int].

   Note that [int #c] implicitly binds a type variable,
   so we need to name it with [as] to get some examples to
   typecheck.
*)
type t = int #c;;
[%%expect {|
Line 1, characters 0-15:
1 | type t = int #c;;
    ^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In type "int #c as 'a" the variable "'a" is unbound
|}];;
type t = C of int #c;;
[%%expect {|
Line 1, characters 0-20:
1 | type t = C of int #c;;
    ^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In case "C of (int #c as 'a)" the variable "'a" is unbound
|}];;
type 'a t = (int #c as 'a);;
let f (_ : int #c) = ();;
type 'a t = C of (int #c as 'a);;
type t = C : int #c -> t;;
[%%expect {|
type 'a t = 'a constraint 'a = int #c
val f : int #c -> unit = <fun>
type 'a t = C of 'a constraint 'a = int #c
type t = C : int #c -> t
|}];;

(* Syntax: int # c
   Interpreted as type application of [#c] to [int].

   Note that [int #c] implicitly binds a type variable,
   so we need to name it with [as] to get some examples to
   typecheck.
*)

type t = int # c;;
[%%expect {|
Line 1, characters 0-16:
1 | type t = int # c;;
    ^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In type "int #c as 'a" the variable "'a" is unbound
|}];;
type t = C of int # c;;
[%%expect {|
Line 1, characters 0-21:
1 | type t = C of int # c;;
    ^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In case "C of (int #c as 'a)" the variable "'a" is unbound
|}];;

type 'a t = (int # c as 'a);;
let f (_ : int # c) = ();;
type 'a t = C of (int # c as 'a);;
type t = C : int # c -> t;;
[%%expect {|
type 'a t = 'a constraint 'a = int #c
val f : int #c -> unit = <fun>
type 'a t = C of 'a constraint 'a = int #c
type t = C : int #c -> t
|}];;

(***************************)
(* Type application: it's a type error, not a parse error. *)

type t = int int#;;
[%%expect {|
Line 1, characters 9-17:
1 | type t = int int#;;
             ^^^^^^^^
Error: The type constructor "int#" expects 0 argument(s),
       but is here applied to 1 argument(s)
|}];;

type t = (int, int) int#;;
[%%expect {|
Line 1, characters 9-24:
1 | type t = (int, int) int#;;
             ^^^^^^^^^^^^^^^
Error: The type constructor "int#" expects 0 argument(s),
       but is here applied to 2 argument(s)
|}];;
