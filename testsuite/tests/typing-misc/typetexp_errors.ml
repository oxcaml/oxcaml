(* TEST
   expect;
*)

type ('a, 'at, 'any, 'en) t = A of 'an

[%%expect
{|
Line 1, characters 35-38:
1 | type ('a, 'at, 'any, 'en) t = A of 'an
                                       ^^^
Error: The type variable "'an" is unbound in this type declaration.
Hint: Did you mean "'a", "'any", "'at" or "'en"?
|}]

type mismatched =
  [< `A of int
  | `B of float > `B
  `C ]

[%%expect
{|
Lines 2-4, characters 2-6:
2 | ..[< `A of int
3 |   | `B of float > `B
4 |   `C ]
Error: The constructor "`C" is missing from the upper bound (between "<" and ">")
       of this polymorphic variant but is present in
       its lower bound (after ">").
       Hint: Either add "`C" in the upper bound, or remove it
       from the lower bound.
|}]

type '_a underscored = A of '_a

[%%expect
{|
Line 1, characters 5-8:
1 | type '_a underscored = A of '_a
         ^^^
Error: The type variable name "'_a" is not allowed in programs
|}]

(* The next two hit the unification error case at the end of
   Typetexp.globalize_used_variables. *)
let f (x : int as 'a) (y : float as 'a) = x, y

[%%expect
{|
Line 1, characters 27-38:
1 | let f (x : int as 'a) (y : float as 'a) = x, y
                               ^^^^^^^^^^^
Error: This type "float" should be an instance of type "int"
|}]

type 'a t1 = 'a constraint 'a = 'b list

type 'a t2 = 'a constraint 'a = 'b option

let f (x : 'a t1) : 'a t2 = assert false

[%%expect
{|
type 'a t1 = 'a constraint 'a = 'b list
type 'a t2 = 'a constraint 'a = 'b option
Line 5, characters 20-22:
5 | let f (x : 'a t1) : 'a t2 = assert false
                        ^^
Error: This type "'a option" should be an instance of type "'b list"
|}]
