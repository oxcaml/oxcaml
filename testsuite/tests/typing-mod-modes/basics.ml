(* TEST
   flags = "-w -220";
   expect;
*)

(* An illegally captured closure can be passed around but not called. *)
let f (g : unit -> unit) = let h @ portable = fun () -> let _ = g in () in h
[%%expect{|
val f : (unit -> unit) @ portable -> unit -> unit = <fun>
|}]

let f (g : unit -> unit) = let h @ portable = fun () -> g () in h
[%%expect{|
val f : (unit -> unit) @ portable -> unit -> unit = <fun>
|}]

let f (g : unit -> unit) = let h @ portable = fun () -> g in h
[%%expect{|
val f : (unit -> unit) @ portable -> unit -> unit -> unit = <fun>
|}]

(* The empty list stays [mod_contended] inside a portable closure. *)
let f () =
  let x = [] in
  let h @ portable = fun () -> (x : int ref list @ uncontended) in h
[%%expect{|
val f : unit -> unit -> int ref list @ contended = <fun>
|}]

let f () = let x = ref 0 in let h @ portable = fun () -> x := 1 in h
[%%expect{|
Line 1, characters 57-58:
1 | let f () = let x = ref 0 in let h @ portable = fun () -> x := 1 in h
                                                             ^
Error: This value is "contended"
         because it is used inside the function at line 1, characters 47-63
         which is expected to be "portable".
       However, the highlighted expression is expected to be "uncontended".
|}]

let f (x : int ref) = let h @ portable = fun () -> x := 1 in h
[%%expect{|
val f : int ref @ mod_contended -> unit -> unit = <fun>
|}]

let f (x @ mod_portable) = x ()
[%%expect{|
Line 1, characters 27-28:
1 | let f (x @ mod_portable) = x ()
                               ^
Error: This value is "mod_portable" but is expected to be "nonportable".
|}]

let f (x @ mod_contended) = x
[%%expect{|
val f : 'a @ mod_contended -> 'a = <fun>
|}]
