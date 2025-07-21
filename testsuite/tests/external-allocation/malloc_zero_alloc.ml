(* TEST
  expect;
*)

(* Tuple allocation *)
let [@zero_alloc] tuple_alloc x y = malloc_ (x,y)
[%%expect{|
val tuple_alloc : 'a @ external_ -> 'b @ external_ -> ('a * 'b) mallocd
  [@@zero_alloc] = <fun>
|}]

(* Record allocation *)
type record_t = {x : int; y : int}
let [@zero_alloc] record_alloc x y = malloc_ {x; y}
[%%expect{|
type record_t = { x : int; y : int; }
val record_alloc : int -> int -> record_t mallocd [@@zero_alloc] = <fun>
|}]

(* Record with unboxed field *)
type mixed_record = {a : int; b : int64#}
let [@zero_alloc] mixed_record_alloc a b = malloc_ {a; b}
[%%expect{|
type mixed_record = { a : int; b : int64#; }
val mixed_record_alloc : int -> int64# -> mixed_record mallocd [@@zero_alloc] =
  <fun>
|}]

(* Variant allocation *)
type variant_t = Foo | Bar of int
let [@zero_alloc] variant_alloc x = malloc_ (Bar x)
[%%expect{|
type variant_t = Foo | Bar of int
val variant_alloc : int -> variant_t mallocd [@@zero_alloc] = <fun>
|}]

(* Option allocation *)
let [@zero_alloc] option_alloc x = malloc_ (Some x)
[%%expect{|
val option_alloc : 'a @ external_ -> 'a option mallocd [@@zero_alloc] = <fun>
|}]

(* List cons allocation *)
let [@zero_alloc] list_cons_alloc x xs = malloc_ (x :: xs)
[%%expect{|
val list_cons_alloc :
  'a @ external_ -> 'a list @ external_ -> 'a list mallocd [@@zero_alloc] =
  <fun>
|}]

(* Polymorphic variant allocation *)
let [@zero_alloc] poly_variant_alloc x = malloc_ (`Tag x)
[%%expect{|
val poly_variant_alloc : 'a @ external_ -> [> `Tag of 'a ] mallocd
  [@@zero_alloc] = <fun>
|}]

(* GADT allocation *)
type 'a gadt_t = Pack : 'a -> 'a gadt_t
let [@zero_alloc] gadt_alloc x = malloc_ (Pack x)
[%%expect{|
type 'a gadt_t = Pack : 'a -> 'a gadt_t
val gadt_alloc : 'a @ external_ -> 'a gadt_t mallocd [@@zero_alloc] = <fun>
|}]
