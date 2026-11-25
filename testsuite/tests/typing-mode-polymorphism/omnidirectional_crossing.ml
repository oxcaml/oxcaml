(* TEST
 flags += "-extension omnidirectionality -extension mode_polymorphism_alpha \
           -extension mode_polymorphism_printing";
 expect;
*)

(* Omnidirectional mode crossing with mode polymorphism. A pending crossing
   guards the mode variables it captures: when generalizing, they (and the
   mode variables related to them) are partially generalized. Instances copy
   them, and once the crossing is known, the changes it makes to them are
   forwarded to the instances (incremental instantiation). *)

let global_sink (_ @ global) = ()

let local_sink (_ @ local) = ()

[%%expect{|
val global_sink : 'a @ [< global] -> unit @ 'm = <fun>
val local_sink : 'a @ [> local] -> unit @ 'm = <fun>
|}]

let f1 (local_ y) =
  let g () = y in
  let _ = y + 1 in
  g ()

[%%expect{|
val f1 : int @ [> local] -> int @ [> dynamic] = <fun>
|}]

(* An instance of [g] is used before the crossing is known. *)
let f2 (local_ y) =
  let g () = y in
  global_sink (g ());
  let _ = y + 1 in
  ()

[%%expect{|
val f2 : int @ [> local] -> unit @ 'm = <fun>
|}]

(* The instance of [g] below is made before the crossing is known, so it only
   sees the crossing through forwarding. *)
let f3 (local_ y) =
  let g () = y in
  global_sink (g ());
  let _ = (y : int ref) in
  ()

[%%expect{|
Line 3, characters 14-20:
3 |   global_sink (g ());
                  ^^^^^^
Error: This value is "local" but is expected to be "global".
|}]

let f4 (local_ y) =
  let g () = y in
  let _ = g () in
  global_sink (g ());
  let _ = (y : int ref) in
  ()

[%%expect{|
Line 4, characters 14-20:
4 |   global_sink (g ());
                  ^^^^^^
Error: This value is "local" but is expected to be "global".
|}]

(* The crossing is defaulted at top level. *)
let f5 (local_ y) =
  let g () = y in
  global_sink (g ())

[%%expect{|
Line 3, characters 14-20:
3 |   global_sink (g ())
                  ^^^^^^
Error: This value is "local" but is expected to be "global".
|}]

(* [g] is still polymorphic in the modes of [x]. *)
let f6 (local_ y) (z @ global) =
  let g x =
    let _ = y in
    x
  in
  global_sink (g z);
  local_sink (g (local_ ref 0));
  let _ = y + 1 in
  ()

[%%expect{|
val f6 : int @ [> local] -> 'a @ [< global] -> unit @ 'm = <fun>
|}]

(* Nested generalization: the instance of [h] in [g] is rooted by [h]'s live
   partial generics, so is partially generalized with [g]. *)
let f7 (local_ y) =
  let g () =
    let h () = y in
    h ()
  in
  global_sink (g ());
  let _ = y + 1 in
  ()

[%%expect{|
val f7 : int @ [> local] -> unit @ 'm = <fun>
|}]

let f8 (local_ y) =
  let g () =
    let h () = y in
    h ()
  in
  global_sink (g ());
  let _ = (y : int ref) in
  ()

[%%expect{|
Line 3, characters 15-16:
3 |     let h () = y in
                   ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

(* The partial generics of [g] are related to mode variables of [k], which are
   generalized while the crossing is still unknown: they are reachable from a
   root (the crossing), so are partially generalized with [k]. *)
let f9 (local_ y) =
  let k () =
    let g () = (y, 0) in
    let (a, _) = g () in
    a
  in
  global_sink (k ());
  let _ = y + 1 in
  ()

[%%expect{|
val f9 : int @ [> local] -> unit @ 'm = <fun>
|}]

let f10 (local_ y) =
  let k () =
    let g () = (y, 0) in
    let (a, _) = g () in
    a
  in
  global_sink (k ());
  let _ = (y : int ref) in
  ()

[%%expect{|
Line 3, characters 15-21:
3 |     let g () = (y, 0) in
                   ^^^^^^
Error: This value is "local"
         because it is allocated at line 3, characters 15-21 containing data
         which is "local" to the parent region
         because it is a tuple that contains the expression at line 3, characters 16-17
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]
