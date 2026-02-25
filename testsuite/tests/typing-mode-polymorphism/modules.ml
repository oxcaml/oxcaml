(* TEST
 flags += "-extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

let use_portable (x @ portable) = x
[%%expect{|
val use_portable : 'a @ [< 'm & portable] -> 'a @ [> 'm] = <fun>
|}]

module M = struct
  let id x = x
end
[%%expect{|
module M : sig val id : 'a -> 'a end
|}]

let a (x @ portable) = use_portable    (M.id x)
let b (x @ nonportable) = use_portable (M.id x)
[%%expect{|
Line 1, characters 39-47:
1 | let a (x @ portable) = use_portable    (M.id x)
                                           ^^^^^^^^
Error: This value is "nonportable" but is expected to be "portable".
|}]
