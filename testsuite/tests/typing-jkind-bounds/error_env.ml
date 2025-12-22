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
Error: This expression has type "'a * 'b"
       but an expression was expected of type "('c : value mod portable)"
       The kind of 'a * 'b is immutable_data with 'a with 'b
         because it's a tuple type.
       But the kind of 'a * 'b must be a subkind of value mod portable
         because of the definition of require_portable at line 1, characters 21-56.
|}]

let f a b = require_portable (a, b, c, d, e)
[%%expect {|
Line 1, characters 29-44:
1 | let f a b = require_portable (a, b, c, d, e)
                                 ^^^^^^^^^^^^^^^
Error: This expression has type "'a * 'b * 'c * 'd * 'e"
       but an expression was expected of type "('f : value mod portable)"
       The kind of 'a * 'b * 'c * 'd * 'e is
           immutable_data with 'a with 'b with 'c with 'd with 'e
         because it's a tuple type.
       But the kind of 'a * 'b * 'c * 'd * 'e must be a subkind of
           value mod portable
         because of the definition of require_portable at line 1, characters 21-56.
|}]

let f a b = require_portable ((a, b), (c, d))
[%%expect {|
Line 1, characters 29-45:
1 | let f a b = require_portable ((a, b), (c, d))
                                 ^^^^^^^^^^^^^^^^
Error: This expression has type "'a * 'b"
       but an expression was expected of type "('c : value mod portable)"
       The kind of 'a * 'b is immutable_data with 'a with 'b
         because it's a tuple type.
       But the kind of 'a * 'b must be a subkind of value mod portable
         because of the definition of require_portable at line 1, characters 21-56.
|}]

type ('a, 'b) t = { a : 'a; b : 'b }
let f a b = require_portable (a, b)
[%%expect {|
type ('a, 'b) t = { a : 'a; b : 'b; }
Line 2, characters 29-35:
2 | let f a b = require_portable (a, b)
                                 ^^^^^^
Error: This expression has type "'a * 'b"
       but an expression was expected of type "('c : value mod portable)"
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
Error: This expression has type "'a * 'b"
       but an expression was expected of type "('c : value mod portable)"
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
Error: This expression has type "'a * 'b"
       but an expression was expected of type "('c : value mod portable)"
       The kind of 'a * 'b is immutable_data with 'a with 'b
         because it's a tuple type.
       But the kind of 'a * 'b must be a subkind of value mod portable
         because of the definition of require_portable at line 1, characters 21-56.
|}]
