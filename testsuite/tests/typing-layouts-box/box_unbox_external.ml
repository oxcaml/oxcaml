(* TEST
   flags = "-extension small_numbers";
   include stdlib_upstream_compatible;
   expect;
*)

(* Declare the primitives *)
external box : ('a : any). 'a -> 'a box_ = "%box"
[@@layout_poly];;

external unbox : ('a : any). 'a box_ -> 'a = "%unbox"
[@@layout_poly];;
[%%expect{|
external box : ('a : any). 'a -> 'a box_ = "%box" [@@layout_poly]
external unbox : ('a : any). 'a box_ -> 'a = "%unbox" [@@layout_poly]
|}]

(* Test 1: box on unboxed types *)
let box_float64 (x : float#) : float# box_ = box x;;
[%%expect{|
val box_float64 : float# -> float# box_ = <fun>
|}]

let box_int32 (x : int32#) : int32# box_ = box x;;
[%%expect{|
val box_int32 : int32# -> int32# box_ = <fun>
|}]

let box_int64 (x : int64#) : int64# box_ = box x;;
[%%expect{|
val box_int64 : int64# -> int64# box_ = <fun>
|}]

let box_nativeint (x : nativeint#) : nativeint# box_ = box x;;
[%%expect{|
val box_nativeint : nativeint# -> nativeint# box_ = <fun>
|}]

(* Test 2: box result unifies with boxed type *)
let use_as_float (x : float#) : float = box x;;
[%%expect{|
val use_as_float : float# -> float = <fun>
|}]

let use_as_int32 (x : int32#) : int32 = box x;;
[%%expect{|
val use_as_int32 : int32# -> int32 = <fun>
|}]

let use_as_int64 (x : int64#) : int64 = box x;;
[%%expect{|
val use_as_int64 : int64# -> int64 = <fun>
|}]

(* Test 3: unbox from boxed types - the return type determines behavior *)
let unbox_float (x : float) : float# = unbox x;;
[%%expect{|
val unbox_float : float -> float# = <fun>
|}]

let unbox_int32 (x : int32) : int32# = unbox x;;
[%%expect{|
val unbox_int32 : int32 -> int32# = <fun>
|}]

let unbox_int64 (x : int64) : int64# = unbox x;;
[%%expect{|
val unbox_int64 : int64 -> int64# = <fun>
|}]

let unbox_nativeint (x : nativeint) : nativeint# = unbox x;;
[%%expect{|
val unbox_nativeint : nativeint -> nativeint# = <fun>
|}]

(* Test 4: roundtrip - box then unbox *)
let roundtrip_float (x : float#) : float# = unbox (box x);;
[%%expect{|
val roundtrip_float : float# -> float# = <fun>
|}]

let roundtrip_int32 (x : int32#) : int32# = unbox (box x);;
[%%expect{|
val roundtrip_int32 : int32# -> int32# = <fun>
|}]

let roundtrip_int64 (x : int64#) : int64# = unbox (box x);;
[%%expect{|
val roundtrip_int64 : int64# -> int64# = <fun>
|}]

(* Test 5: box/unbox on value types *)
let box_value (x : int) : int box_ = box x
let unbox_value (x : int box_) : int = unbox x;;
[%%expect{|
val box_value : int -> int box_ = <fun>
val unbox_value : int box_ -> int = <fun>
|}]

(* Test 6: box/unbox on unit (value type) *)
let box_unit () : unit box_ = box ();;
[%%expect{|
val box_unit : unit -> unit box_ = <fun>
|}]

(* Test 7: float32# boxing/unboxing *)
let box_float32 (x : float32#) : float32# box_ = box x;;
[%%expect{|
val box_float32 : float32# -> float32# box_ = <fun>
|}]

let use_as_float32 (x : float32#) : float32 = box x;;
[%%expect{|
val use_as_float32 : float32# -> float32 = <fun>
|}]

let unbox_float32 (x : float32) : float32# = unbox x;;
[%%expect{|
val unbox_float32 : float32 -> float32# = <fun>
|}]

let roundtrip_float32 (x : float32#) : float32# = unbox (box x);;
[%%expect{|
val roundtrip_float32 : float32# -> float32# = <fun>
|}]

(* Test 8: void - proper void type boxing/unboxing *)
type void : void
external void : unit -> void = "%unbox_unit";;
[%%expect{|
type void : void
external void : unit -> void = "%unbox_unit"
|}]

let box_void (v : void) : void box_ = box v;;
[%%expect{|
val box_void : void -> void box_ = <fun>
|}]

let unbox_void (v : void box_) : void = unbox v;;
[%%expect{|
val unbox_void : void box_ -> void = <fun>
|}]

let roundtrip_void (v : void) : void = unbox (box v);;
[%%expect{|
val roundtrip_void : void -> void = <fun>
|}]

(* Test 9: nested box_ - boxing an already boxed type *)
(* float# box_ is a value type, so boxing it again should work via identity *)
let double_box (x : float#) : float# box_ box_ = box (box x);;
[%%expect{|
val double_box : float# -> float# box_ box_ = <fun>
|}]

let double_unbox (x : float# box_ box_) : float# = unbox (unbox x);;
[%%expect{|
val double_unbox : float# box_ box_ -> float# = <fun>
|}]

(* Test 10: nested box_ roundtrip *)
let roundtrip_double (x : float#) : float# = double_unbox (double_box x);;
[%%expect{|
val roundtrip_double : float# -> float# = <fun>
|}]

(* Test 11: product sorts should error *)
type p = #(int * string);;
[%%expect{|
type p = #(int * string)
|}]

let box_product (x : p) = box x;;
[%%expect{|
Line 1, characters 26-31:
1 | let box_product (x : p) = box x;;
                              ^^^^^
Error: Unknown builtin primitive "%box"
|}]

let unbox_product (x : p box_) = unbox x;;
[%%expect{|
Line 1, characters 33-40:
1 | let unbox_product (x : p box_) = unbox x;;
                                     ^^^^^^^
Error: Unknown builtin primitive "%unbox"
|}]
