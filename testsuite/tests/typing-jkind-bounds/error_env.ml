(* TEST
 expect;
*)

let require_portable (_ : (_ : value mod portable)) = ()
[%%expect {|
val require_portable : ('a : value mod portable). 'a -> unit = <fun>
|}]

let f a b = require_portable (a, b)
[%%expect {|
val f :
  ('a : value_or_null mod portable) ('b : value_or_null mod portable).
    'a -> 'b -> unit =
  <fun>
|}]

let f a b = require_portable (a, b, c, d, e)
[%%expect {|
Line 1, characters 36-37:
1 | let f a b = require_portable (a, b, c, d, e)
                                        ^
Error: Unbound value "c"
|}]

let f a b = require_portable ((a, b), (c, d))
[%%expect {|
Line 1, characters 39-40:
1 | let f a b = require_portable ((a, b), (c, d))
                                           ^
Error: Unbound value "c"
|}]

type ('a, 'b) t = { a : 'a; b : 'b }
let f a b = require_portable (a, b)
[%%expect {|
type ('a, 'b) t = { a : 'a; b : 'b; }
val f :
  ('a : value_or_null mod portable) ('b : value_or_null mod portable).
    'a -> 'b -> unit =
  <fun>
|}]

type ('a, 'b) t = Foo of 'a * 'b
let f a b = require_portable (a, b)
[%%expect {|
type ('a, 'b) t = Foo of 'a * 'b
val f :
  ('a : value_or_null mod portable) ('b : value_or_null mod portable).
    'a -> 'b -> unit =
  <fun>
|}]

let f (a : _ list) (b : _ option) = require_portable (a, b)
[%%expect {|
val f :
  ('a : value_or_null mod portable) ('b : value_or_null mod portable).
    'a list -> 'b option -> unit =
  <fun>
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
val f : ('a : value mod portable). unit -> 'a t_with_bound -> unit = <fun>
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
