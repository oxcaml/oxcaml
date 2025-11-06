(* TEST
   expect;
*)

external ( ! ) : 'a ref @ shared -> 'a @@ portable = "%field0"

[%%expect{|
external ( ! ) : 'a ref @ shared -> 'a = "%field0"
|}]

(* Closing over use of shared gives splittable *)

let foo () =
    let a = ref 0 in
    let bar () = !a in
    let _ @ splittable = bar in
    ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let foo () =
  let a = ref 0 in
  let bar () = a := 1 in
  let _ @ splittable = bar in
  ()

[%%expect{|
Line 4, characters 23-26:
4 |   let _ @ splittable = bar in
                           ^^^
Error: This value is "nonportable"
       because it contains a usage (of the value "a" at Line 3, characters 15-16)
       which is expected to be "uncontended".
       However, the highlighted expression is expected to be "splittable".
|}]

(* Closing over a splittable value also gives splittable. *)

let foo (f : (unit -> unit) @ splittable) @ splittable = fun () -> f ()
[%%expect{|
val foo : (unit -> unit) @ splittable -> (unit -> unit) @ splittable = <fun>
|}]

let foo (f : (unit -> unit) @ splittable) @ nonportable = fun () -> f ()
[%%expect{|
val foo : (unit -> unit) @ splittable -> unit -> unit = <fun>
|}]

let foo (f : (unit -> unit) @ splittable) @ portable = fun () -> f ()
[%%expect{|
Line 1, characters 65-66:
1 | let foo (f : (unit -> unit) @ splittable) @ portable = fun () -> f ()
                                                                     ^
Error: The value "f" is "splittable" but is expected to be "portable"
       because it is used inside the function at Line 1, characters 55-69
       which is expected to be "portable".
|}]

let foo (f : (unit -> unit) @ nonportable) @ splittable = fun () -> f ()
[%%expect{|
Line 1, characters 68-69:
1 | let foo (f : (unit -> unit) @ nonportable) @ splittable = fun () -> f ()
                                                                        ^
Error: The value "f" is "nonportable" but is expected to be "splittable"
       because it is used inside the function at Line 1, characters 58-72
       which is expected to be "splittable".
|}]

let foo (f : (unit -> unit) @ portable) @ splittable = fun () -> f ()
[%%expect{|
val foo : (unit -> unit) @ portable -> (unit -> unit) @ splittable = <fun>
|}]
