(* TEST
   flags = "-w -220";
   expect;
*)

(* Acceptance is exact (accept iff satisfiable) and does not depend on the
   order in which constraints are met. *)

(* The illegally captured [g] flows out of the closure; nothing calls it. *)
let f (g : unit -> unit) =
  let h @ portable = fun () -> g in
  h
[%%expect{|
val f : (unit -> unit) @ portable -> unit -> unit -> unit = <fun>
|}]

(* A parameter defaults to its old mode when that is feasible. *)
let f (g : unit -> unit) =
  let h @ portable = fun () -> g in
  (h ()) ()
[%%expect{|
val f : (unit -> unit) @ portable -> unit = <fun>
|}]

(* Calling an illegally captured nonportable closure is rejected, whatever
   the order of the constraints. *)
let f () =
  let r = ref 0 in
  let g () = incr r in
  let h @ portable = fun () -> g in
  (h ()) ()
[%%expect{|
Line 5, characters 2-8:
5 |   (h ()) ()
      ^^^^^^
Error: This value is "mod_portable" but is expected to be "nonportable".
|}]

let f () =
  let r = ref 0 in
  let g () = incr r in
  let k = (fun () -> g) in
  let use (h @ portable) = h in
  (use k) ()
[%%expect{|
val f : unit -> (unit -> unit) @ mod_portable = <fun>
|}]

let f () =
  let r = ref 0 in
  let g () = incr r in
  let k = (fun () -> g) in
  let x = k () in
  let use (h @ portable) = h in
  let _ = use k in
  x ()
[%%expect{|
Line 7, characters 10-15:
7 |   let _ = use k in
              ^^^^^
Warning 5 [ignored-partial-application]: this function application is partial,
  maybe some arguments are missing.

Line 8, characters 2-3:
8 |   x ()
      ^
Error: This value is "mod_portable" but is expected to be "nonportable".
|}]

(* Order independence: the capture comes first, the legal use later. A legal
   use later does not make the earlier capture fail. *)
let f () =
  let g = fun () -> () in
  let h @ portable = fun () -> let _ = g in () in
  let (_ : unit -> unit) = g in
  h
[%%expect{|
val f : unit -> unit -> unit = <fun>
|}]

(* The captured value is only illegal if it is really nonportable. *)
let f () =
  let g = fun () -> () in
  let h @ portable = fun () -> g () in
  h
[%%expect{|
val f : unit -> unit -> unit = <fun>
|}]

let f () =
  let r = ref 0 in
  let g = fun () -> incr r in
  let h @ portable = fun () -> g () in
  h
[%%expect{|
Line 4, characters 31-32:
4 |   let h @ portable = fun () -> g () in
                                   ^
Error: The value "g" is "nonportable"
         because it contains a usage (of the value "r" at line 3, characters 25-26)
         which is expected to be "uncontended".
       However, the value "g" highlighted is expected to be "portable"
         because it is used inside the function at line 4, characters 21-35
         which is expected to be "portable".
|}]

(* Nested closures: the inner closure is mod_portable inside the outer. *)
let f () =
  let r = ref 0 in
  let g () = incr r in
  let h @ portable = fun () -> let k () = g in k in
  h
[%%expect{|
val f : unit -> unit -> unit -> (unit -> unit) @ mod_portable = <fun>
|}]

let f () =
  let r = ref 0 in
  let g () = incr r in
  let h @ portable = fun () -> let k () = g () in k in
  h
[%%expect{|
Line 4, characters 42-43:
4 |   let h @ portable = fun () -> let k () = g () in k in
                                              ^
Error: The value "g" is "nonportable"
         because it contains a usage (of the value "r" at line 3, characters 18-19)
         which is expected to be "uncontended".
       However, the value "g" highlighted is expected to be "portable"
         because it is used inside the function at line 4, characters 21-51
         which is expected to be "portable".
|}]

(* Shareable and corruptible (the eight). *)
let f () =
  let r = ref 0 in
  let g @ shareable = fun () -> ignore r.contents in
  let h @ portable = fun () -> g in
  h
[%%expect{|
val f : unit -> unit -> (unit -> unit) @ mod_portable = <fun>
|}]

let f () =
  let r = ref 0 in
  let g @ shareable = fun () -> ignore r.contents in
  let h @ shareable = fun () -> g () in
  h
[%%expect{|
val f : unit -> unit -> unit = <fun>
|}]

let f () =
  let r = ref 0 in
  let g @ shareable = fun () -> ignore r.contents in
  let h @ corruptible = fun () -> g () in
  h
[%%expect{|
Line 4, characters 34-35:
4 |   let h @ corruptible = fun () -> g () in
                                      ^
Error: The value "g" is "shareable"
       but is expected to be "corruptible"
         because it is used inside the function at line 4, characters 24-38
         which is expected to be "corruptible".
|}]

(* Explicit mod annotations are sources. *)
let f (x : int ref @ mod_contended) = x
[%%expect{|
val f : int ref @ mod_contended -> int ref = <fun>
|}]

let f (x : int ref @ mod_contended) = x := 1
[%%expect{|
val f : int ref @ mod_contended -> unit = <fun>
|}]

let f (x : int ref @ mod_contended) = let h @ portable = fun () -> x := 1 in h
[%%expect{|
val f : int ref @ mod_contended -> unit -> unit = <fun>
|}]

let f (g @ mod_portable) = g
[%%expect{|
val f : 'a @ mod_portable -> 'a @ mod_portable = <fun>
|}]

let f (g @ mod_portable) = g ()
[%%expect{|
Line 1, characters 27-28:
1 | let f (g @ mod_portable) = g ()
                               ^
Error: This value is "mod_portable" but is expected to be "nonportable".
|}]

(* Without an annotation, the parameter is inferred [mod_contended]: the
   function is accepted but cannot be applied to a real [int ref]. *)
let f (x : int ref) = let h @ portable = fun () -> x := 1 in h
[%%expect{|
val f : int ref @ mod_contended -> unit -> unit = <fun>
|}]

let _ = f (ref 0)
[%%expect{|
Line 1, characters 10-17:
1 | let _ = f (ref 0)
              ^^^^^^^
Error: This value is "uncontended" but is expected to be "mod_contended".
|}]

(* Statefulness / visibility. *)
let f () =
  let r = ref 0 in
  let g = fun () -> incr r in
  let h @ stateless = fun () -> g in
  h
[%%expect{|
val f : unit -> unit -> (unit -> unit) @ mod_stateless = <fun>
|}]

let f () =
  let r = ref 0 in
  let g = fun () -> incr r in
  let h @ stateless = fun () -> g () in
  h
[%%expect{|
Line 4, characters 32-33:
4 |   let h @ stateless = fun () -> g () in
                                    ^
Error: The value "g" is "stateful"
         because it contains a usage (of the value "r" at line 3, characters 25-26)
         which is expected to be "read_write".
       However, the value "g" highlighted is expected to be "stateless"
         because it is used inside the function at line 4, characters 22-36
         which is expected to be "stateless".
|}]
