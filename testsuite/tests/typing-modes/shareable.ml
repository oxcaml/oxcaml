(* TEST
   expect;
*)

external ( ! ) : 'a ref @ shared -> 'a @@ portable = "%field0"

[%%expect{|
external ( ! ) : 'a ref @ shared -> 'a = "%field0"
|}]

(* Closing over use of shared gives shareable *)

let foo () =
    let a = ref 0 in
    let bar () = !a in
    let _ @ shareable = bar in
    ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let foo () =
  let a = ref 0 in
  let bar () = a := 1 in
  let _ @ shareable = bar in
  ()

[%%expect{|
Line 4, characters 22-25:
4 |   let _ @ shareable = bar in
                          ^^^
Error: This value is "nonportable"
         because it contains a usage (of the value "a" at line 3, characters 15-16)
         which is expected to be "uncontended".
       However, the highlighted expression is expected to be "shareable".
|}]

(* Closing over a shareable value also gives shareable. *)

let foo (f : (unit -> unit) @ shareable) @ shareable = fun () -> f ()
[%%expect{|
val foo : (unit -> unit) @ shareable -> (unit -> unit) @ shareable = <fun>
|}]

let foo (f : (unit -> unit) @ shareable) @ nonportable = fun () -> f ()
[%%expect{|
val foo : (unit -> unit) @ shareable -> unit -> unit = <fun>
|}]

let foo (f : (unit -> unit) @ shareable) @ portable = fun () -> f ()
[%%expect{|
Line 1, characters 64-65:
1 | let foo (f : (unit -> unit) @ shareable) @ portable = fun () -> f ()
                                                                    ^
Error: The value "f" is "shareable"
       but is expected to be "portable"
         because it is used inside the function at line 1, characters 54-68
         which is expected to be "portable".
|}]

let foo (f : (unit -> unit) @ nonportable) @ shareable = fun () -> f ()
[%%expect{|
Line 1, characters 67-68:
1 | let foo (f : (unit -> unit) @ nonportable) @ shareable = fun () -> f ()
                                                                       ^
Error: The value "f" is "nonportable"
       but is expected to be "shareable"
         because it is used inside the function at line 1, characters 57-71
         which is expected to be "shareable".
|}]

let foo (f : (unit -> unit) @ portable) @ shareable = fun () -> f ()
[%%expect{|
val foo : (unit -> unit) @ portable -> (unit -> unit) @ shareable = <fun>
|}]

(* Modality. *)

type 'a t = { x : 'a @@ shareable }

let get : 'a t -> 'a @ shareable = fun t -> t.x

[%%expect{|
type 'a t = { x : 'a @@ shareable; }
val get : 'a t -> 'a @ shareable = <fun>
|}]

let get : 'a t -> 'a @ portable = fun t -> t.x

[%%expect{|
Line 1, characters 43-46:
1 | let get : 'a t -> 'a @ portable = fun t -> t.x
                                               ^^^
Error: This value is "shareable"
         because it is the field "x" of the record at line 1, characters 43-44
         which is "shareable" because it crosses with something
         which is "nonportable".
       However, the highlighted expression is expected to be "portable".
|}]

(* Crossing *)

type cross_shareable : value mod shareable

[%%expect{|
type cross_shareable : value mod shareable
|}]

let cross_shareable1 (x : cross_shareable @ shareable) : _ @ portable = x

[%%expect{|
Line 1, characters 72-73:
1 | let cross_shareable1 (x : cross_shareable @ shareable) : _ @ portable = x
                                                                            ^
Error: This value is "shareable" but is expected to be "portable".
|}]

let cross_shareable2 (x : cross_shareable @ nonportable) : _ @ shareable = x

[%%expect{|
val cross_shareable2 : cross_shareable -> cross_shareable @ shareable = <fun>
|}]

type t

type s : value mod shareable = { v : t @@ shareable } [@@unboxed]
type u : value mod corruptible = { v : t @@ corruptible } [@@unboxed]

[%%expect{|
type t
type s = { v : t @@ shareable; } [@@unboxed]
type u = { v : t @@ corruptible; } [@@unboxed]
|}]
