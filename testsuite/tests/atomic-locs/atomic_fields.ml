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
type t = { x : int; mutable y : int; }
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
