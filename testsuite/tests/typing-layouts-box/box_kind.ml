(* TEST
   include stdlib_upstream_compatible;
   expect;
*)

(* Test 1: Basic box_kind syntax for abstract types *)

type t : bits32 box_kind;;
[%%expect{|
type t : bits32 box_kind mod non_float
|}]

type t2 : bits64 box_kind;;
[%%expect{|
type t2 : bits64 box_kind mod non_float
|}]

type t3 : float64 box_kind;;
[%%expect{|
type t3 : float64 box_kind mod non_float
|}]

(* Test 2: box_kind types are subtypes of value - can be used as function args *)

type t4 : bits32 box_kind
let f (x : t4) = x;;
[%%expect{|
type t4 : bits32 box_kind mod non_float
val f : t4 -> t4 = <fun>
|}]

(* Test 3: box_kind of product kinds *)

type t5 : (bits32 & bits64) box_kind;;
[%%expect{|
type t5 : (bits32 & bits64) box_kind mod non_float
|}]

(* Test 3b: product box_kind types are subtypes of value *)
type t5b : (bits32 & bits64) box_kind
let store_product (x : t5b) = [x];;
[%%expect{|
type t5b : (bits32 & bits64) box_kind mod non_float
val store_product : t5b -> t5b list = <fun>
|}]

(* Test 3c: different product box_kinds don't unify *)
type t5c : (bits32 & bits64) box_kind
type t5d : (bits64 & bits32) box_kind
let f (x : t5c) : t5d = x;;
[%%expect{|
type t5c : (bits32 & bits64) box_kind mod non_float
type t5d : (bits64 & bits32) box_kind mod non_float
Line 3, characters 24-25:
3 | let f (x : t5c) : t5d = x;;
                            ^
Error: This expression has type "t5c" but an expression was expected of type
         "t5d"
|}]

(* Test 4: Nested box_kind is allowed syntactically *)

type t6 : bits32 box_kind box_kind;;
[%%expect{|
type t6 : bits32 box_kind box_kind mod non_float
|}]

(* Test 5: box_kind with mode modifiers *)

type t7 : bits32 box_kind mod global;;
[%%expect{|
type t7 : bits32 box_kind mod global non_float
|}]

(* Test 6: box_kind is a subkind of value - can be stored in a list *)

type t8 : bits32 box_kind
let store (x : t8) = [x];;
[%%expect{|
type t8 : bits32 box_kind mod non_float
val store : t8 -> t8 list = <fun>
|}]

(* Test 7: box_kind can satisfy value constraint *)

type t9 : bits32 box_kind
type ('a : value) box_wrapper = { contents : 'a }
let wrap (x : t9) : t9 box_wrapper = { contents = x };;
[%%expect{|
type t9 : bits32 box_kind mod non_float
type 'a box_wrapper = { contents : 'a; }
val wrap : t9 -> t9 box_wrapper = <fun>
|}]

(* Test 8: Different box_kind types don't unify *)

type t10 : bits32 box_kind
type t11 : bits64 box_kind
let f (x : t10) : t11 = x;;
[%%expect{|
type t10 : bits32 box_kind mod non_float
type t11 : bits64 box_kind mod non_float
Line 3, characters 24-25:
3 | let f (x : t10) : t11 = x;;
                            ^
Error: This expression has type "t10" but an expression was expected of type
         "t11"
|}]
