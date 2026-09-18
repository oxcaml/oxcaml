(* TEST
 expect;
*)

let require_portable (_ : (_ : value mod portable)) = ()
[%%expect {|
val require_portable : ('a : value mod portable). 'a -> unit = <fun>
|}]

let f a b = require_portable (a, b)
[%%expect {|
Line 1, characters 29-35:
1 | let f a b = require_portable (a, b)
                                 ^^^^^^
Error:
       The kind of 'a * 'b is immutable_data with 'a with 'b
         because it's a tuple type.
       But the kind of 'a * 'b must be a subkind of value mod portable
         because of the definition of require_portable at line 1, characters 21-56.
|}]

let f a b c d e = require_portable (a, b, c, d, e)
[%%expect {|
Line 1, characters 35-50:
1 | let f a b c d e = require_portable (a, b, c, d, e)
                                       ^^^^^^^^^^^^^^^
Error:
       The kind of 'a * 'b * 'c * 'd * 'e is
           immutable_data with 'a with 'b with 'c with 'd with 'e
         because it's a tuple type.
       But the kind of 'a * 'b * 'c * 'd * 'e must be a subkind of
           value mod portable
         because of the definition of require_portable at line 1, characters 21-56.
|}]

let f a b c d = require_portable ((a, b), (c, d))
[%%expect {|
Line 1, characters 33-49:
1 | let f a b c d = require_portable ((a, b), (c, d))
                                     ^^^^^^^^^^^^^^^^
Error:
       The kind of ('a * 'b) * ('c * 'd) is
           immutable_data with 'a with 'b with 'c with 'd
         because it's a tuple type.
       But the kind of ('a * 'b) * ('c * 'd) must be a subkind of
           value mod portable
         because of the definition of require_portable at line 1, characters 21-56.
|}, Principal{|
Line 1, characters 33-49:
1 | let f a b c d = require_portable ((a, b), (c, d))
                                     ^^^^^^^^^^^^^^^^
Error:
       The kind of ('a * 'b) * ('c * 'd) is
           immutable_data with 'a * 'b with 'c * 'd
         because it's a tuple type.
       But the kind of ('a * 'b) * ('c * 'd) must be a subkind of
           value mod portable
         because of the definition of require_portable at line 1, characters 21-56.
|}]

type ('a, 'b) t = { a : 'a; b : 'b }
let f a b = require_portable (a, b)
[%%expect {|
type ('a, 'b) t = { a : 'a; b : 'b; }
Line 2, characters 29-35:
2 | let f a b = require_portable (a, b)
                                 ^^^^^^
Error:
       The kind of 'a * 'b is immutable_data with 'a with 'b
         because it's a tuple type.
       But the kind of 'a * 'b must be a subkind of value mod portable
         because of the definition of require_portable at line 1, characters 21-56.
|}]

type ('a, 'b) t = Foo of 'a * 'b
let f a b = require_portable (a, b)
[%%expect {|
type ('a, 'b) t = Foo of 'a * 'b
Line 2, characters 29-35:
2 | let f a b = require_portable (a, b)
                                 ^^^^^^
Error:
       The kind of 'a * 'b is immutable_data with 'a with 'b
         because it's a tuple type.
       But the kind of 'a * 'b must be a subkind of value mod portable
         because of the definition of require_portable at line 1, characters 21-56.
|}]

let f (a : _ list) (b : _ option) = require_portable (a, b)
[%%expect {|
Line 1, characters 53-59:
1 | let f (a : _ list) (b : _ option) = require_portable (a, b)
                                                         ^^^^^^
Error:
       The kind of 'a list * 'b option is immutable_data with 'a with 'b
         because it's a tuple type.
       But the kind of 'a list * 'b option must be a subkind of
           value mod portable
         because of the definition of require_portable at line 1, characters 21-56.
|}, Principal{|
Line 1, characters 53-59:
1 | let f (a : _ list) (b : _ option) = require_portable (a, b)
                                                         ^^^^^^
Error:
       The kind of 'a list * 'b option is
           immutable_data with 'a list with 'b option
         because it's a tuple type.
       But the kind of 'a list * 'b option must be a subkind of
           value mod portable
         because of the definition of require_portable at line 1, characters 21-56.
|}]

type 'a t_no_bound = unit
type 'a t_with_bound = 'a option
let id x = x
let f (a : _ t_no_bound) (b : _ t_with_bound) =
  require_portable (id (a, b))
(* CR layouts: in the non-principal case, the jkind should be [with 'b] rather
   than [with 'a]. Internal ticket 6133. *)
[%%expect {|
type 'a t_no_bound = unit
type 'a t_with_bound = 'a option
val id : 'a -> 'a = <fun>
Line 5, characters 19-30:
5 |   require_portable (id (a, b))
                       ^^^^^^^^^^^
Error: This expression has type "unit * 'a t_with_bound"
       but an expression was expected of type "('b : value mod portable)"
       The kind of unit * 'a t_with_bound is immutable_data with 'a
         because it's a tuple type.
       But the kind of unit * 'a t_with_bound must be a subkind of
           value mod portable
         because of the definition of require_portable at line 1, characters 21-56.
|}, Principal{|
type 'a t_no_bound = unit
type 'a t_with_bound = 'a option
val id : 'a -> 'a = <fun>
Line 5, characters 19-30:
5 |   require_portable (id (a, b))
                       ^^^^^^^^^^^
Error: This expression has type "unit * 'a t_with_bound"
       but an expression was expected of type "('b : value mod portable)"
       The kind of unit * 'a t_with_bound is
           immutable_data with 'a t_with_bound with unit
         because it's a tuple type.
       But the kind of unit * 'a t_with_bound must be a subkind of
           value mod portable
         because of the definition of require_portable at line 1, characters 21-56.
|}]
