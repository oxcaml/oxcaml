(* TEST
   expect;
*)

type t = {
  x : int;
  mutable y : int [@atomic];
}

let get_y (t : t) = t.y
let set_y (t : t) new_y = t.y <- new_y
[%%expect{|
type t = { x : int; mutable y : int [@atomic]; }
val get_y : t -> int = <fun>
val set_y : t -> int -> unit = <fun>
|}]

let t : t = { x = 1; y = 2 }
[%%expect{|
val t : t = {x = 1; y = 2}
|}]

let () = Format.printf "%d@." (get_y t)
[%%expect{|
2
|}]

let () = set_y t 7
[%%expect{|
|}]

let () = Format.printf "%d@." (get_y t)
[%%expect{|
7
|}]

(* Test with non-immediates too *)

type u = {
  x : int;
  mutable y : string [@atomic];
}

let get_y (t : u) = t.y
let set_y (t : u) new_y = t.y <- new_y
[%%expect{|
type u = { x : int; mutable y : string [@atomic]; }
val get_y : u -> string = <fun>
val set_y : u -> string -> unit = <fun>
|}]

let t : u = { x = 1; y = "two" }
[%%expect{|
val t : u = {x = 1; y = "two"}
|}]

let () = Format.printf "%s@." (get_y t)
[%%expect{|
two
|}]

let () = set_y t "seven"
[%%expect{|
|}]

let () = Format.printf "%s@." (get_y t)
[%%expect{|
seven
|}]
