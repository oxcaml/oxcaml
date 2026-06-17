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
