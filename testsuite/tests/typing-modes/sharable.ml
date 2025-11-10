(* TEST
   expect;
*)

external ( ! ) : 'a ref @ shared -> 'a @@ portable = "%field0"

[%%expect{|
external ( ! ) : 'a ref @ shared -> 'a = "%field0"
|}]

(* Closing over use of shared gives sharable *)

let foo () =
    let a = ref 0 in
    let bar () = !a in
    let _ @ sharable = bar in
    ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let foo () =
  let a = ref 0 in
  let bar () = a := 1 in
  let _ @ sharable = bar in
  ()

[%%expect{|
Line 4, characters 21-24:
4 |   let _ @ sharable = bar in
                         ^^^
Error: This value is "nonportable"
       because it contains a usage (of the value "a" at Line 3, characters 15-16)
       which is expected to be "uncontended".
       However, the highlighted expression is expected to be "sharable".
|}]

(* Closing over a sharable value also gives sharable. *)

let foo (f : (unit -> unit) @ sharable) @ sharable = fun () -> f ()
[%%expect{|
val foo : (unit -> unit) @ sharable -> (unit -> unit) @ sharable = <fun>
|}]

let foo (f : (unit -> unit) @ sharable) @ nonportable = fun () -> f ()
[%%expect{|
val foo : (unit -> unit) @ sharable -> unit -> unit = <fun>
|}]

let foo (f : (unit -> unit) @ sharable) @ portable = fun () -> f ()
[%%expect{|
Line 1, characters 63-64:
1 | let foo (f : (unit -> unit) @ sharable) @ portable = fun () -> f ()
                                                                   ^
Error: The value "f" is "sharable" but is expected to be "portable"
       because it is used inside the function at Line 1, characters 53-67
       which is expected to be "portable".
|}]

let foo (f : (unit -> unit) @ nonportable) @ sharable = fun () -> f ()
[%%expect{|
Line 1, characters 66-67:
1 | let foo (f : (unit -> unit) @ nonportable) @ sharable = fun () -> f ()
                                                                      ^
Error: The value "f" is "nonportable" but is expected to be "sharable"
       because it is used inside the function at Line 1, characters 56-70
       which is expected to be "sharable".
|}]

let foo (f : (unit -> unit) @ portable) @ sharable = fun () -> f ()
[%%expect{|
val foo : (unit -> unit) @ portable -> (unit -> unit) @ sharable = <fun>
|}]

(* Modality. *)

type 'a t = { x : 'a @@ sharable }

let get : 'a t -> 'a @ sharable = fun t -> t.x

[%%expect{|
type 'a t = { x : 'a @@ sharable; }
val get : 'a t -> 'a @ sharable = <fun>
|}]

let get : 'a t -> 'a @ portable = fun t -> t.x

[%%expect{|
Line 1, characters 43-46:
1 | let get : 'a t -> 'a @ portable = fun t -> t.x
                                               ^^^
Error: This value is "sharable" but is expected to be "portable".
|}]

(* Crossing *)

type cross_sharable : value mod sharable

[%%expect{|
type cross_sharable : value mod sharable
|}]

let cross_sharable1 (x : cross_sharable @ sharable) : _ @ portable = x

[%%expect{|
Line 1, characters 69-70:
1 | let cross_sharable1 (x : cross_sharable @ sharable) : _ @ portable = x
                                                                         ^
Error: This value is "sharable" but is expected to be "portable".
|}]

let cross_sharable2 (x : cross_sharable @ nonportable) : _ @ sharable = x

[%%expect{|
val cross_sharable2 : cross_sharable -> cross_sharable @ sharable = <fun>
|}]

type t

(* Doesn't work yet. *)
type s : value mod sharable = { v : t @@ sharable } [@@unboxed]

[%%expect{|
type t
Line 4, characters 0-63:
4 | type s : value mod sharable = { v : t @@ sharable } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "s" is value
         because of the definition of t at line 1, characters 0-6.
       But the kind of type "s" must be a subkind of value mod sharable
         because of the annotation on the declaration of the type s.
|}]