(* TEST
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

(* Test 6: void - boxing void produces a value *)
let box_void () : unit box_ = box ();;
[%%expect{|
val box_void : unit -> unit box_ = <fun>
|}]
