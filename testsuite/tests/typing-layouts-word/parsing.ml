(* TEST
 expect;
*)

(* These tests show how potential ambiguities are resolved
   between the types #c and nativeint_u.
*)

(* Basic syntax: nativeint_u is an unboxed nativeint. *)
type t = nativeint_u;;
let f (_ : nativeint_u) = ();;
[%%expect {|
type t = nativeint_u
val f : nativeint_u -> unit = <fun>
|}];;

type t = C of nativeint_u;;
[%%expect {|
type t = C of nativeint_u
|}];;

type t = C : nativeint_u -> t;;
[%%expect {|
type t = C : nativeint_u -> t
|}];;

(* nativeint_u works as an argument to normal type constructors, not just
   classes, even though many of the rest of the tests in this file are concerned
   with classes.
*)
type t = nativeint_u list;;
[%%expect {|
Line 1, characters 9-20:
1 | type t = nativeint_u list;;
             ^^^^^^^^^^^
Error: This type "nativeint_u" should be an instance of type
         "('a : value_or_null)"
       The layout of nativeint_u is word
         because it is the primitive type nativeint_u.
       But the layout of nativeint_u must be a value layout
         because the type argument of list has layout value_or_null.
|}];;

let f (_ : nativeint_u list) = ();;
[%%expect {|
Line 1, characters 11-22:
1 | let f (_ : nativeint_u list) = ();;
               ^^^^^^^^^^^
Error: This type "nativeint_u" should be an instance of type
         "('a : value_or_null)"
       The layout of nativeint_u is word
         because it is the primitive type nativeint_u.
       But the layout of nativeint_u must be a value layout
         because the type argument of list has layout value_or_null.
|}];;

type t = C of nativeint_u list;;
[%%expect {|
Line 1, characters 14-25:
1 | type t = C of nativeint_u list;;
                  ^^^^^^^^^^^
Error: This type "nativeint_u" should be an instance of type
         "('a : value_or_null)"
       The layout of nativeint_u is word
         because it is the primitive type nativeint_u.
       But the layout of nativeint_u must be a value layout
         because the type argument of list has layout value_or_null.
|}];;

type t = C : nativeint_u list -> t;;
[%%expect {|
Line 1, characters 13-24:
1 | type t = C : nativeint_u list -> t;;
                 ^^^^^^^^^^^
Error: This type "nativeint_u" should be an instance of type
         "('a : value_or_null)"
       The layout of nativeint_u is word
         because it is the primitive type nativeint_u.
       But the layout of nativeint_u must be a value layout
         because the type argument of list has layout value_or_null.
|}];;

(* Syntax: nativeint_uc
   Interpreted as type application of [c] to [nativeint_u].
*)
class ['a] c = object(self)
  method x :'a = assert false
end;;
[%%expect {|
class ['a] c : object method x : 'a end
|}];;

type t = nativeint_uc;;
[%%expect {|
Line 1, characters 9-21:
1 | type t = nativeint_uc;;
             ^^^^^^^^^^^^
Error: Unbound type constructor "nativeint_uc"
Hint: Did you mean "nativeint_u"?
|}];;

let f (_ : nativeint_uc) = ();;
[%%expect {|
Line 1, characters 11-23:
1 | let f (_ : nativeint_uc) = ();;
               ^^^^^^^^^^^^
Error: Unbound type constructor "nativeint_uc"
Hint: Did you mean "nativeint_u"?
|}];;

type t = C of nativeint_uc;;
[%%expect {|
Line 1, characters 14-26:
1 | type t = C of nativeint_uc;;
                  ^^^^^^^^^^^^
Error: Unbound type constructor "nativeint_uc"
Hint: Did you mean "nativeint_u"?
|}];;

type t = C : nativeint_uc -> t;;
[%%expect {|
Line 1, characters 13-25:
1 | type t = C : nativeint_uc -> t;;
                 ^^^^^^^^^^^^
Error: Unbound type constructor "nativeint_uc"
Hint: Did you mean "nativeint_u"?
|}];;

(* Syntax: nativeint_u c
   Interpreted as type application of [c] to [nativeint_u].
*)
type t = nativeint_u c;;
[%%expect {|
Line 1, characters 9-20:
1 | type t = nativeint_u c;;
             ^^^^^^^^^^^
Error: This type "nativeint_u" should be an instance of type "('a : value)"
       The layout of nativeint_u is word
         because it is the primitive type nativeint_u.
       But the layout of nativeint_u must be a value layout
         because it's a type argument to a class constructor.
|}];;

let f (_ : nativeint_u c) = ();;
[%%expect {|
Line 1, characters 11-22:
1 | let f (_ : nativeint_u c) = ();;
               ^^^^^^^^^^^
Error: This type "nativeint_u" should be an instance of type "('a : value)"
       The layout of nativeint_u is word
         because it is the primitive type nativeint_u.
       But the layout of nativeint_u must be a value layout
         because it's a type argument to a class constructor.
|}];;

type t = C of nativeint_u c;;
[%%expect {|
Line 1, characters 14-25:
1 | type t = C of nativeint_u c;;
                  ^^^^^^^^^^^
Error: This type "nativeint_u" should be an instance of type "('a : value)"
       The layout of nativeint_u is word
         because it is the primitive type nativeint_u.
       But the layout of nativeint_u must be a value layout
         because it's a type argument to a class constructor.
|}];;

type t = C : nativeint_u c -> t;;
[%%expect {|
Line 1, characters 13-24:
1 | type t = C : nativeint_u c -> t;;
                 ^^^^^^^^^^^
Error: This type "nativeint_u" should be an instance of type "('a : value)"
       The layout of nativeint_u is word
         because it is the primitive type nativeint_u.
       But the layout of nativeint_u must be a value layout
         because it's a type argument to a class constructor.
|}];;

(* Syntax: nativeint #c
   Interpreted as type application of [#c] to [nativeint].

   Note that [nativeint #c] implicitly binds a type variable,
   so we need to name it with [as] to get some examples to
   typecheck.
*)
type t = nativeint #c;;
[%%expect {|
Line 1, characters 0-21:
1 | type t = nativeint #c;;
    ^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In type "nativeint #c as 'a" the variable "'a" is unbound
|}];;
type t = C of nativeint #c;;
[%%expect {|
Line 1, characters 0-26:
1 | type t = C of nativeint #c;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In case "C of (nativeint #c as 'a)" the variable "'a" is unbound
|}];;
type 'a t = (nativeint #c as 'a);;
let f (_ : nativeint #c) = ();;
type 'a t = C of (nativeint #c as 'a);;
type t = C : nativeint #c -> t;;
[%%expect {|
type 'a t = 'a constraint 'a = nativeint #c
val f : nativeint #c -> unit = <fun>
type 'a t = C of 'a constraint 'a = nativeint #c
type t = C : nativeint #c -> t
|}];;

(* Syntax: nativeint # c
   Interpreted as type application of [#c] to [nativeint].

   Note that [nativeint #c] implicitly binds a type variable,
   so we need to name it with [as] to get some examples to
   typecheck.
*)

type t = nativeint # c;;
[%%expect {|
Line 1, characters 0-22:
1 | type t = nativeint # c;;
    ^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In type "nativeint #c as 'a" the variable "'a" is unbound
|}];;
type t = C of nativeint # c;;
[%%expect {|
Line 1, characters 0-27:
1 | type t = C of nativeint # c;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In case "C of (nativeint #c as 'a)" the variable "'a" is unbound
|}];;

type 'a t = (nativeint # c as 'a);;
let f (_ : nativeint # c) = ();;
type 'a t = C of (nativeint # c as 'a);;
type t = C : nativeint # c -> t;;
[%%expect {|
type 'a t = 'a constraint 'a = nativeint #c
val f : nativeint #c -> unit = <fun>
type 'a t = C of 'a constraint 'a = nativeint #c
type t = C : nativeint #c -> t
|}];;

(***************************)
(* Type application: it's a type error, not a parse error. *)

type t = int nativeint_u;;
[%%expect {|
Line 1, characters 9-24:
1 | type t = int nativeint_u;;
             ^^^^^^^^^^^^^^^
Error: The type constructor "nativeint_u" expects 0 argument(s),
       but is here applied to 1 argument(s)
|}];;

type t = (int, int) nativeint_u;;
[%%expect {|
Line 1, characters 9-31:
1 | type t = (int, int) nativeint_u;;
             ^^^^^^^^^^^^^^^^^^^^^^
Error: The type constructor "nativeint_u" expects 0 argument(s),
       but is here applied to 2 argument(s)
|}];;
