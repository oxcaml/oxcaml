(* TEST
 expect;
*)

type t = int8x16;;
[%%expect{|
type t = int8x16
|}];;

type t = int16x8;;
[%%expect{|
type t = int16x8
|}];;

type t = int32x4;;
[%%expect{|
type t = int32x4
|}];;

type t = int64x2;;
[%%expect{|
type t = int64x2
|}];;

type t = float32x4;;
[%%expect{|
type t = float32x4
|}];;

type t = float64x2;;
[%%expect{|
type t = float64x2
|}];;

type t = int8x64;;
[%%expect{|
type t = int8x64
|}];;

type t = int16x32;;
[%%expect{|
type t = int16x32
|}];;

type t = int32x16;;
[%%expect{|
type t = int32x16
|}];;

type t = int64x8;;
[%%expect{|
type t = int64x8
|}];;

type t = float16x32;;
[%%expect{|
type t = float16x32
|}];;

type t = float32x16;;
[%%expect{|
type t = float32x16
|}];;

type t = float64x8;;
[%%expect{|
type t = float64x8
|}];;

type t = mask;;
[%%expect{|
type t = mask
|}];;

type t = mask#;;
[%%expect{|
type t = mask#
|}];;

type t_mask : mask
type ('a : mask) id = 'a;;
[%%expect{|
type t_mask : mask
type ('a : mask) id = 'a
|}];;

let f (x : t_mask) = x;;
[%%expect{|
val f : t_mask -> t_mask = <fun>
|}];;

type t = mask# array;;
[%%expect{|
type t = mask# array
|}];;

let f (a : mask# array) : mask# array = a;;
[%%expect{|
val f : mask# array -> mask# array = <fun>
|}];;

(* [@unboxed] is supported on [mask]. *)
external id : (mask[@unboxed]) -> (mask[@unboxed]) = "id_byte" "id_native";;
[%%expect{|
external id : mask -> mask = "id_byte" "id_native" [@@unboxed]
|}];;
