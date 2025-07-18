(* TEST
 expect;
*)

let immediate_is_external () =
  let _x @ external_ = 3 in
  let _y @ external_ = 'a' in
  ()
[%%expect{|
val immediate_is_external : unit -> unit = <fun>
|}]

let immediate_is_external64 () =
  let _x @ external64 = 3 in
  let _y @ external64 = 'a' in
  let _z @ external64 = None in
  ()
[%%expect{|
val immediate_is_external64 : unit -> unit = <fun>
|}]

let none_is_external () =
  let _x @ external_ = None in
  ()
[%%expect {|
val none_is_external : unit -> unit = <fun>
|}]

let empty_list_is_external () =
  let _x @ external_ = [] in
  ()
[%%expect {|
val empty_list_is_external : unit -> unit = <fun>
|}]


let some_is_not_external () =
  let _y @ external_ = Some 3 in
  ()
[%%expect {|
Line 2, characters 23-29:
2 |   let _y @ external_ = Some 3 in
                           ^^^^^^
Error: This value is "internal" but expected to be "external_".
|}]

let float_is_not_external x =
  let u : float @ external_ = (3.0 +. x) in
  u
[%%expect{|
Line 2, characters 30-40:
2 |   let u : float @ external_ = (3.0 +. x) in
                                  ^^^^^^^^^^
Error: This value is "internal" but expected to be "external_".
|}]


type t = {a : int; b : float}
let record_is_not_external () =
  let _y @ external_ = {a = 3; b = 4.0} in
  ()
[%%expect{|
type t = { a : int; b : float; }
Line 3, characters 23-39:
3 |   let _y @ external_ = {a = 3; b = 4.0} in
                           ^^^^^^^^^^^^^^^^
Error: This value is "internal" but expected to be "external_".
|}]

let cannot_pass_internal_to_external =
  let foo (x : 'a @ external_) = () in
  let bar (x : 'a @ internal) = foo x in
  ()
[%%expect{|
Line 3, characters 36-37:
3 |   let bar (x : 'a @ internal) = foo x in
                                        ^
Error: This value is "internal" but expected to be "external_".
|}]

let cannot_pass_internal_to_external64 =
  let foo (x : 'a @ external64) = () in
  let bar (x : 'a @ internal) = foo x in
  ()

[%%expect{|
Line 3, characters 36-37:
3 |   let bar (x : 'a @ internal) = foo x in
                                        ^
Error: This value is "internal" but expected to be "external64".
|}]



(*
   The code below is the expected behavior. For future axes where
   the legacy mode is top, you are allowed to do this. Similarly you can write a
   nonportable thing into a portable record.  You're just prevented from
   creating a portable record with a mutable field not marked as @@ portable.
   (see tests in [malloc.ml])
 *)

type 'a t = {mutable f : 'a}
let foo (t : 'a t @ external_) (x : 'a @ internal) =
  t.f <- x
[%%expect{|
type 'a t = { mutable f : 'a; }
val foo : 'a t -> 'a -> unit = <fun>
|}]

let foo (t : 'a ref @ external_) (x : 'a @ internal) =
  t := x
[%%expect{|
val foo : 'a ref -> 'a -> unit = <fun>
|}]

let foo (t : 'a array @ external_) (x : 'a @ internal) =
  t.(0) <- x
[%%expect{|
val foo : 'a array -> 'a -> unit = <fun>
|}]

type 'a t = {mutable f : 'a @@ external_}
let bar (t : 'a t @ external_) (x : 'a @ internal) =
  t.f <- x
[%%expect{|
type 'a t = { mutable f : 'a @@ external_; }
Line 3, characters 9-10:
3 |   t.f <- x
             ^
Error: This value is "internal" but expected to be "external_".
|}]


(* CR jcutler: In the above case, creating an @external_ record of this type
   should be *allowed*. *)

type 'a external_ = { ext : 'a @@ external_ }
let into_external () =
  let _ : int option external_ = {ext = Some 3} in
  ()
[%%expect {|
type 'a external_ = { ext : 'a @@ external_; }
Line 3, characters 40-46:
3 |   let _ : int option external_ = {ext = Some 3} in
                                            ^^^^^^
Error: This value is "internal" but expected to be "external_".
|}]
