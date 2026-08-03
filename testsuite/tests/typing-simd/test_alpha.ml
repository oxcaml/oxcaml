(* TEST
 flags = "-extension simd_alpha";
 expect;
*)

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
