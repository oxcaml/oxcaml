(* TEST
 expect;
*)

let my_unforkable : (unit -> unit) @ unforkable = print_endline "Hello, world!"
[%%expect{|
Line 1, characters 4-79:
1 | let my_unforkable : (unit -> unit) @ unforkable = print_endline "Hello, world!"
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "unforkable" but is expected to be "spawnable".
|}]

let storage = ref ""

let with_unforkable : ((string -> unit) @ local unforkable -> 'a) -> 'a =
  fun f -> f ((:=) storage)

[%%expect{|
val storage : string ref = {contents = ""}
val with_unforkable : ((string -> unit) @ local -> 'a) -> 'a = <fun>
|}]

let () = with_unforkable (fun k -> k "Hello, world!")

let _ = !storage

[%%expect{|
- : string = "Hello, world!"
|}]

let run_unforkable : (string -> unit) @ local unforkable -> unit = fun f -> f "my string"

let () = with_unforkable (fun k -> run_unforkable k)

let _ = !storage

[%%expect{|
val run_unforkable : (string -> unit) @ local -> unit = <fun>
- : string = "my string"
|}]


let run_forkable : (string -> unit) @ local forkable -> unit = fun f -> f "another string"

let () = with_unforkable (fun k -> run_forkable k)

[%%expect{|
val run_forkable : (string -> unit) @ local forkable -> unit = <fun>
Line 3, characters 48-49:
3 | let () = with_unforkable (fun k -> run_forkable k)
                                                    ^
Error: This value is "unforkable" but is expected to be "forkable".
|}]

let run_spawnable : (string -> unit) @ local spawnable -> unit = fun f -> f "another string"

let () = with_unforkable (fun k -> run_spawnable k)

[%%expect{|
val run_spawnable : (string -> unit) @ local spawnable -> unit = <fun>
Line 3, characters 49-50:
3 | let () = with_unforkable (fun k -> run_spawnable k)
                                                     ^
Error: This value is "unforkable" but is expected to be "spawnable".
|}]

let run_default : (string -> unit) @ local -> unit = fun f -> f "some string"

let () = with_unforkable (fun k -> run_default k)

[%%expect{|
val run_default : (string -> unit) @ local -> unit = <fun>
|}]

(* A closure over a [unforkable] value must be [unforkable]. *)

let () = with_unforkable (fun k ->
  let closure @ local spawnable = fun () -> k () in
  run_forkable k)

[%%expect{|
Line 2, characters 44-45:
2 |   let closure @ local spawnable = fun () -> k () in
                                                ^
Error: The value "k" is "unforkable" but is expected to be "spawnable"
       because it is used inside the function at Line 2, characters 34-48
       which is expected to be "spawnable".
|}]

let () = with_unforkable (fun k ->
  let closure @ local spawnable = fun () -> k () in
  run_spawnable k)

[%%expect{|
Line 2, characters 44-45:
2 |   let closure @ local spawnable = fun () -> k () in
                                                ^
Error: The value "k" is "unforkable" but is expected to be "spawnable"
       because it is used inside the function at Line 2, characters 34-48
       which is expected to be "spawnable".
|}]


type 'a t0 = Mk0 of 'a @@ global
type 'a t1 = Mk1 of 'a @@ global spawnable
type 'a t2 = Mk2 of 'a @@ global unforkable
type 'a t3 = Mk3 of 'a @@ global spawnable unyielding
type 'a t4 = Mk4 of 'a @@ global spawnable yielding
type 'a t5 = Mk5 of 'a @@ global unforkable unyielding
type 'a t6 = Mk6 of 'a @@ global unforkable yielding
type 'a t7 = Mk7 of 'a @@ spawnable unyielding
type 'a t8 = Mk8 of 'a @@ spawnable yielding
type 'a t9 = Mk9 of 'a @@ unforkable unyielding
type 'a t10 = Mk10 of 'a @@ unforkable yielding
type 'a t11 = Mk11 of 'a @@ spawnable
type 'a t12 = Mk12 of 'a @@ unforkable
type 'a t13 = Mk13 of 'a @@ forkable
type 'a t14 = Mk14 of 'a @@ global forkable

let with_global_unforkable : ((string -> unit) @ unforkable -> 'a) -> 'a =
  fun f -> f ((:=) storage)

[%%expect{|
type 'a t0 = Mk0 of 'a @@ global
type 'a t1 = Mk1 of 'a @@ global
type 'a t2 = Mk2 of 'a @@ global unforkable
type 'a t3 = Mk3 of 'a @@ global
type 'a t4 = Mk4 of 'a @@ global yielding
type 'a t5 = Mk5 of 'a @@ global unforkable
type 'a t6 = Mk6 of 'a @@ global unforkable yielding
type 'a t7 = Mk7 of 'a @@ spawnable unyielding
type 'a t8 = Mk8 of 'a @@ spawnable
type 'a t9 = Mk9 of 'a @@ unyielding
type 'a t10 = Mk10 of 'a
type 'a t11 = Mk11 of 'a @@ spawnable
type 'a t12 = Mk12 of 'a
type 'a t13 = Mk13 of 'a @@ forkable
type 'a t14 = Mk14 of 'a @@ global forkable
val with_global_unforkable : ((string -> unit) @ unforkable -> 'a) -> 'a =
  <fun>
|}]

(* [global] modality implies [spawnable]. *)
let _ = with_global_unforkable (fun k -> let _ = Mk0 k in ())

[%%expect{|
Line 1, characters 53-54:
1 | let _ = with_global_unforkable (fun k -> let _ = Mk0 k in ())
                                                         ^
Error: This value is "unforkable" but is expected to be "spawnable".
|}]

(* [global unforkable] works: *)
let _ = with_global_unforkable (fun k -> let _ = Mk2 k in ())

[%%expect{|
- : unit = ()
|}]

(* [unforkable]/[forkable]/[spawnable] modalities: *)
let _ = with_global_unforkable (fun k -> let _ = Mk11 k in ())

[%%expect{|
Line 1, characters 54-55:
1 | let _ = with_global_unforkable (fun k -> let _ = Mk11 k in ())
                                                          ^
Error: This value is "unforkable" but is expected to be "spawnable".
|}]

let _ = with_global_unforkable (fun k -> let _ = Mk12 k in ())

[%%expect{|
- : unit = ()
|}]

let _ = with_global_unforkable (fun k -> let _ = Mk13 k in ())

[%%expect{|
Line 1, characters 54-55:
1 | let _ = with_global_unforkable (fun k -> let _ = Mk13 k in ())
                                                          ^
Error: This value is "unforkable" but is expected to be "forkable".
|}]

(* Externals and [yielding]: *)

external ok_unforkable : 'a @ local -> unit = "%ignore"

let _ = ok_unforkable 4

let _ = ok_unforkable (stack_ (Some "local string"))

let _ = with_global_unforkable (fun k -> ok_unforkable k)

[%%expect{|
external ok_unforkable : 'a @ local -> unit = "%ignore"
- : unit = ()
- : unit = ()
- : unit = ()
|}]

external requires_forkable : 'a @ local forkable -> unit = "%ignore"

let _ = requires_forkable 4

let _ = requires_forkable (stack_ (Some "local string"))

external requires_spawnable : 'a @ local spawnable -> unit = "%ignore"

let _ = requires_spawnable 4

let _ = requires_spawnable (stack_ (Some "local string"))

let _ = with_global_unforkable (fun k -> requires_forkable k)

[%%expect{|
external requires_forkable : 'a @ local forkable -> unit = "%ignore"
- : unit = ()
- : unit = ()
external requires_spawnable : 'a @ local spawnable -> unit = "%ignore"
- : unit = ()
- : unit = ()
Line 13, characters 59-60:
13 | let _ = with_global_unforkable (fun k -> requires_forkable k)
                                                                ^
Error: This value is "unforkable" but is expected to be "forkable".
|}]

external returns_forkable : 'a -> 'a @ local spawnable = "%identity"

let _ = requires_forkable (returns_forkable "some string")

external returns_spawnable : 'a -> 'a @ local spawnable = "%identity"

let _ = requires_spawnable (returns_spawnable "some string")

[%%expect{|
external returns_forkable : 'a -> 'a @ local spawnable = "%identity"
- : unit = ()
external returns_spawnable : 'a -> 'a @ local spawnable = "%identity"
- : unit = ()
|}]

(* [@local_opt] and [spawnable]: *)

external id : ('a[@local_opt]) -> ('a[@local_opt]) = "%identity"

let f1 x = id x
let f2 (x @ local) = exclave_ id x
let f3 (x @ unforkable) = id x
let f4 (x @ local spawnable) = exclave_ id x

[%%expect{|
external id : ('a [@local_opt]) -> ('a [@local_opt]) = "%identity"
val f1 : 'a -> 'a = <fun>
val f2 : 'a @ local -> 'a @ local = <fun>
val f3 : 'a @ unforkable -> 'a @ unforkable = <fun>
val f4 : 'a @ local spawnable -> 'a @ local = <fun>
|}]

(* Test [instance_prim] + mixed mode annots. *)
external requires_forkable : 'a @ local forkable -> (unit [@local_opt]) = "%ignore"

let f1 x = requires_forkable x
[%%expect{|
external requires_forkable : 'a @ local forkable -> (unit [@local_opt])
  = "%ignore"
val f1 : 'a -> unit = <fun>
|}]

let f2 (x @ local) = exclave_ requires_forkable x

[%%expect{|
Line 1, characters 48-49:
1 | let f2 (x @ local) = exclave_ requires_forkable x
                                                    ^
Error: This value is "unforkable" but is expected to be "forkable".
|}]

let f3 (x @ unforkable) = requires_forkable x
[%%expect{|
Line 1, characters 44-45:
1 | let f3 (x @ unforkable) = requires_forkable x
                                                ^
Error: This value is "unforkable" but is expected to be "forkable".
|}]

let f4 (x @ local forkable) = exclave_ requires_forkable x
[%%expect{|
val f4 : 'a @ local forkable -> unit @ local = <fun>
|}]

external requires_spawnable : 'a @ local spawnable -> (unit [@local_opt]) = "%ignore"

let f1 x = requires_spawnable x
[%%expect{|
external requires_spawnable : 'a @ local spawnable -> (unit [@local_opt])
  = "%ignore"
val f1 : 'a -> unit = <fun>
|}]

let f2 (x @ local) = exclave_ requires_spawnable x

[%%expect{|
Line 1, characters 49-50:
1 | let f2 (x @ local) = exclave_ requires_spawnable x
                                                     ^
Error: This value is "unforkable" but is expected to be "spawnable".
|}]

let f3 (x @ unforkable) = requires_spawnable x
[%%expect{|
Line 1, characters 45-46:
1 | let f3 (x @ unforkable) = requires_spawnable x
                                                 ^
Error: This value is "unforkable" but is expected to be "spawnable".
|}]

let f4 (x @ local spawnable) = exclave_ requires_spawnable x
[%%expect{|
val f4 : 'a @ local spawnable -> unit @ local = <fun>
|}]

(* [@local_opt] overrides annotations. *)
external overridden: ('a[@local_opt]) @ local spawnable -> unit = "%ignore"

let succeeds (x @ local) = overridden x
[%%expect{|
external overridden : ('a [@local_opt]) @ local spawnable -> unit = "%ignore"
val succeeds : 'a @ local -> unit = <fun>
|}]

(* [mod global] implies [mod spawnable] by default. *)

type ('a : value mod global) u1

type ('a : value mod global unforkable) u2

type w1 : value mod global unforkable

type w2 : value mod global forkable

type w3 : value mod global spawnable

[%%expect{|
type ('a : value mod global) u1
type ('a : value mod global unforkable) u2
type w1 : value mod global unforkable
type w2 : value mod global forkable forkable
type w3 : value mod global
|}]

type _z1 = w1 u1

[%%expect{|
Line 1, characters 11-13:
1 | type _z1 = w1 u1
               ^^
Error: This type "w1" should be an instance of type "('a : value mod global)"
       The kind of w1 is value mod global unforkable
         because of the definition of w1 at line 5, characters 0-37.
       But the kind of w1 must be a subkind of value mod global
         because of the definition of u1 at line 1, characters 0-31.
|}]

type _z2 = w2 u1

[%%expect{|
Line 1, characters 11-13:
1 | type _z2 = w2 u1
               ^^
Error: This type "w2" should be an instance of type "('a : value mod global)"
       The kind of w2 is value mod global forkable forkable
         because of the definition of w2 at line 7, characters 0-35.
       But the kind of w2 must be a subkind of value mod global
         because of the definition of u1 at line 1, characters 0-31.
|}]

type _z3 = w3 u1

[%%expect{|
type _z3 = w3 u1
|}]

type _z4 = w1 u2

[%%expect{|
type _z4 = w1 u2
|}]

type _z5 = w2 u2

[%%expect{|
type _z5 = w2 u2
|}]

type _z4 = w3 u2

[%%expect{|
type _z4 = w3 u2
|}]
