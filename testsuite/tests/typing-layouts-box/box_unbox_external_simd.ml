(* TEST
   flags = "-extension simd_beta";
   include stdlib_upstream_compatible;
   expect;
*)

(* This file tests %box and %unbox for SIMD vector types.
   SIMD vectors use Pbox_vector/Punbox_vector primitives. *)

external box : ('a : any). 'a -> 'a box_ = "%box"
[@@layout_poly];;

external unbox : ('a : any). 'a box_ -> 'a = "%unbox"
[@@layout_poly];;
[%%expect{|
external box : ('a : any). 'a -> 'a box_ = "%box" [@@layout_poly]
external unbox : ('a : any). 'a box_ -> 'a = "%unbox" [@@layout_poly]
|}]

(* Test 1: vec128 boxing *)
let box_vec128 (x : int64x2#) : int64x2# box_ = box x;;
[%%expect{|
val box_vec128 : int64x2# -> int64x2# box_ = <fun>
|}]

let unbox_vec128 (x : int64x2# box_) : int64x2# = unbox x;;
[%%expect{|
val unbox_vec128 : int64x2# box_ -> int64x2# = <fun>
|}]

let roundtrip_vec128 (x : int64x2#) : int64x2# = unbox (box x);;
[%%expect{|
val roundtrip_vec128 : int64x2# -> int64x2# = <fun>
|}]

(* Test 2: vec128 type unification - int64x2# box_ should equal int64x2 *)
let use_as_int64x2 (x : int64x2#) : int64x2 = box x;;
[%%expect{|
val use_as_int64x2 : int64x2# -> int64x2 = <fun>
|}]

let from_int64x2 (x : int64x2) : int64x2# = unbox x;;
[%%expect{|
val from_int64x2 : int64x2 -> int64x2# = <fun>
|}]

(* Test 3: other vec128 types *)
let box_float64x2 (x : float64x2#) : float64x2# box_ = box x;;
[%%expect{|
val box_float64x2 : float64x2# -> float64x2# box_ = <fun>
|}]

let use_as_float64x2 (x : float64x2#) : float64x2 = box x;;
[%%expect{|
val use_as_float64x2 : float64x2# -> float64x2 = <fun>
|}]

let box_int32x4 (x : int32x4#) : int32x4# box_ = box x;;
[%%expect{|
val box_int32x4 : int32x4# -> int32x4# box_ = <fun>
|}]

let use_as_int32x4 (x : int32x4#) : int32x4 = box x;;
[%%expect{|
val use_as_int32x4 : int32x4# -> int32x4 = <fun>
|}]
