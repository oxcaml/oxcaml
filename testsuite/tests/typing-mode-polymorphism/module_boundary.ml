(* TEST
 flags += "-extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

(* A value bound inside a structure must stay mode-polymorphic: each use of
   [M.id] below instantiates its mode independently. *)

module M = struct
  let id x = x
end
[%%expect{|
module M : sig val id : 'a @ [< 'm] -> 'a @ [> 'm] end
|}]

let need_portable (x @ portable) = x
let need_nonportable (x @ nonportable) = x
[%%expect{|
val need_portable : 'a @ [< 'm & portable] -> 'a @ [> 'm] = <fun>
val need_nonportable : 'a @ [< 'm > nonportable] -> 'a @ [> 'm | nonportable] =
  <fun>
|}]

let a (x @ portable) = need_portable (M.id x)
[%%expect{|
val a : 'a @ [< 'm & global portable] -> 'a @ [> 'm] = <fun>
|}]

let b (x @ nonportable) = need_nonportable (M.id x)
[%%expect{|
val b : 'a @ [< 'm & global > nonportable] -> 'a @ [> 'm | nonportable] =
  <fun>
|}]
