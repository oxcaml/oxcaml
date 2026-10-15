(* TEST
 expect;
*)

let require_portable (_ : (_ : value mod portable)) = ()
[%%expect {|
val require_portable : ('a : value mod portable). 'a -> unit = <fun>
|}]

let f a b (g : int -> int) =
  let x = (a, b, g) in
  require_portable x
[%%expect {|
Line 3, characters 19-20:
3 |   require_portable x
                       ^
Error: The value "x" has type "'a * 'b * (int -> int)"
       but an expression was expected of type "('c : value mod portable)"
       The kind of 'a * 'b * (int -> int) is
           value non_float mod immutable with 'a with 'b
         because it's a tuple type.
       But the kind of 'a * 'b * (int -> int) must be a subkind of
           value mod portable
         because of the definition of require_portable at line 1, characters 21-56.
|}]

let f a b c d e (g : int -> int) =
  let x = (a, b, c, d, e, g) in
  require_portable x
[%%expect {|
Line 3, characters 19-20:
3 |   require_portable x
                       ^
Error: The value "x" has type "'a * 'b * 'c * 'd * 'e * (int -> int)"
       but an expression was expected of type "('f : value mod portable)"
       The kind of 'a * 'b * 'c * 'd * 'e * (int -> int) is
           value non_float
             mod immutable
             with 'a
             with 'b
             with 'c
             with 'd
             with 'e
         because it's a tuple type.
       But the kind of 'a * 'b * 'c * 'd * 'e * (int -> int) must be a subkind of
         value mod portable
         because of the definition of require_portable at line 1, characters 21-56.
|}]

let f a b c d (g : int -> int) =
  let x = ((a, b), (c, d), g) in
  require_portable x
[%%expect {|
Line 3, characters 19-20:
3 |   require_portable x
                       ^
Error: The value "x" has type "('a * 'b) * ('c * 'd) * (int -> int)"
       but an expression was expected of type "('e : value mod portable)"
       The kind of ('a * 'b) * ('c * 'd) * (int -> int) is
           value non_float mod immutable with 'a with 'b with 'c with 'd
         because it's a tuple type.
       But the kind of ('a * 'b) * ('c * 'd) * (int -> int) must be a subkind of
         value mod portable
         because of the definition of require_portable at line 1, characters 21-56.
|}, Principal{|
Line 3, characters 19-20:
3 |   require_portable x
                       ^
Error: The value "x" has type "('a * 'b) * ('c * 'd) * (int -> int)"
       but an expression was expected of type "('e : value mod portable)"
       The kind of ('a * 'b) * ('c * 'd) * (int -> int) is
           value non_float mod immutable with 'a * 'b with 'c * 'd
         because it's a tuple type.
       But the kind of ('a * 'b) * ('c * 'd) * (int -> int) must be a subkind of
         value mod portable
         because of the definition of require_portable at line 1, characters 21-56.
|}]

type ('a, 'b) t = { a : 'a; b : 'b }
let f a b (g : int -> int) =
  let x = (a, b, g) in
  require_portable x
[%%expect {|
type ('a, 'b) t = { a : 'a; b : 'b; }
Line 4, characters 19-20:
4 |   require_portable x
                       ^
Error: The value "x" has type "'a * 'b * (int -> int)"
       but an expression was expected of type "('c : value mod portable)"
       The kind of 'a * 'b * (int -> int) is
           value non_float mod immutable with 'a with 'b
         because it's a tuple type.
       But the kind of 'a * 'b * (int -> int) must be a subkind of
           value mod portable
         because of the definition of require_portable at line 1, characters 21-56.
|}]

type ('a, 'b) t = Foo of 'a * 'b
let f a b (g : int -> int) =
  let x = (a, b, g) in
  require_portable x
[%%expect {|
type ('a, 'b) t = Foo of 'a * 'b
Line 4, characters 19-20:
4 |   require_portable x
                       ^
Error: The value "x" has type "'a * 'b * (int -> int)"
       but an expression was expected of type "('c : value mod portable)"
       The kind of 'a * 'b * (int -> int) is
           value non_float mod immutable with 'a with 'b
         because it's a tuple type.
       But the kind of 'a * 'b * (int -> int) must be a subkind of
           value mod portable
         because of the definition of require_portable at line 1, characters 21-56.
|}]

let f (a : _ list) (b : _ option) (g : int -> int) =
  let x = (a, b, g) in
  require_portable x
[%%expect {|
Line 3, characters 19-20:
3 |   require_portable x
                       ^
Error: The value "x" has type "'a list * 'b option * (int -> int)"
       but an expression was expected of type "('c : value mod portable)"
       The kind of 'a list * 'b option * (int -> int) is
           value non_float mod immutable with 'a with 'b
         because it's a tuple type.
       But the kind of 'a list * 'b option * (int -> int) must be a subkind of
         value mod portable
         because of the definition of require_portable at line 1, characters 21-56.
|}, Principal{|
Line 3, characters 19-20:
3 |   require_portable x
                       ^
Error: The value "x" has type "'a list * 'b option * (int -> int)"
       but an expression was expected of type "('c : value mod portable)"
       The kind of 'a list * 'b option * (int -> int) is
           value non_float mod immutable with 'a list with 'b option
         because it's a tuple type.
       But the kind of 'a list * 'b option * (int -> int) must be a subkind of
         value mod portable
         because of the definition of require_portable at line 1, characters 21-56.
|}]

type 'a t_no_bound = unit
type 'a t_with_bound = 'a option
let id x = x
let f (a : _ t_no_bound) (b : _ t_with_bound) (g : int -> int) =
  require_portable (id (a, b, g))
(* CR layouts: in the non-principal case, the jkind should be [with 'b] rather
   than [with 'a]. Internal ticket 6133. *)
[%%expect {|
type 'a t_no_bound = unit
type 'a t_with_bound = 'a option
val id : 'a -> 'a = <fun>
Line 5, characters 19-33:
5 |   require_portable (id (a, b, g))
                       ^^^^^^^^^^^^^^
Error: This expression has type "unit * 'a t_with_bound * (int -> int)"
       but an expression was expected of type "('b : value mod portable)"
       The kind of unit * 'a t_with_bound * (int -> int) is
           value non_float mod immutable with 'a
         because it's a tuple type.
       But the kind of unit * 'a t_with_bound * (int -> int) must be a subkind of
         value mod portable
         because of the definition of require_portable at line 1, characters 21-56.
|}, Principal{|
type 'a t_no_bound = unit
type 'a t_with_bound = 'a option
val id : 'a -> 'a = <fun>
Line 5, characters 19-33:
5 |   require_portable (id (a, b, g))
                       ^^^^^^^^^^^^^^
Error: This expression has type "unit * 'a t_with_bound * (int -> int)"
       but an expression was expected of type "('b : value mod portable)"
       The kind of unit * 'a t_with_bound * (int -> int) is
           value non_float mod immutable with 'a t_with_bound with unit
         because it's a tuple type.
       But the kind of unit * 'a t_with_bound * (int -> int) must be a subkind of
         value mod portable
         because of the definition of require_portable at line 1, characters 21-56.
|}]
