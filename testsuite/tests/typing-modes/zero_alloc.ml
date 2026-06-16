(* TEST
 flags+="-extension mode_alpha";
 expect;
*)

(* Tests for the Allocation mode axis (modes [alloc] / [noalloc] /
   [noalloc_strict]). *)

type t = { x : float; y : float }
[%%expect{|
type t = { x : float; y : float; }
|}]

(* A function that returns a record. The [@ alloc] mode on the
   binding is the legacy default; the return mode is also default to [@ alloc] since it
   is ok to return the [@ noalloc] type as [@ alloc] *)
let (new_record @ alloc) () =
    ({ x = 3.0; y = 4.0 } : t @ noalloc)
[%%expect{|
val new_record : unit -> t = <fun>
|}]

(* The [@ noalloc] return mode needs to be explicitly specified *)
let f () : t @ noalloc = { x = 3.0; y = 4.0 }
[%%expect{|
val f : unit -> t @ noalloc = <fun>
|}]

(* CR-soon shsong: error expected -- f ishould not be accepted as [@ noalloc]
      Also need to figure out why M is [@@ noalloc_strict] *)
(* The allocation mode shows up in a [val] signature's function type. *)
module M : sig val f : unit -> t @ noalloc end = struct
    let f () = { x = 3.0; y = 4.0 }
end
[%%expect{|
module M : sig val f : unit -> t @ noalloc end @@ stateless noalloc_strict
|}]

(* The allocation mode of a function-typed argument. *)
let apply (g : (unit -> t) @ noalloc) = g
[%%expect{|
val apply : (unit -> t) @ noalloc -> unit -> t = <fun>
|}]

(* Error triggered by mode capture*)
let noalloc_use : 'a @ noalloc -> unit = fun _ -> ()
let (allocates @ alloc) () = { x = 1.; y = 2. }
[%%expect{|
val noalloc_use : 'a @ noalloc -> unit = <fun>
val allocates : unit -> t = <fun>
|}]
let capture_in_closure () = noalloc_use allocates
[%%expect{|
Line 1, characters 40-49:
1 | let capture_in_closure () = noalloc_use allocates
                                            ^^^^^^^^^
Error: This value is "alloc" but is expected to be "noalloc".
|}]

let capture_in_closure () = noalloc_use (fun () -> allocates ())
[%%expect{|
Line 1, characters 51-60:
1 | let capture_in_closure () = noalloc_use (fun () -> allocates ())
                                                       ^^^^^^^^^
Error: The value "allocates" is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at line 1, characters 40-64
         which is expected to be "noalloc".
|}]

let cap (g @ alloc) =
  let h () = g () in
  (h : _ @ noalloc)
[%%expect{|
Line 3, characters 3-4:
3 |   (h : _ @ noalloc)
       ^
Error: This value is "alloc"
         because it closes over the value "g" at line 2, characters 13-14
         which is "alloc".
       However, the highlighted expression is expected to be "noalloc".
|}]

let (allocates2 @ alloc) () = ()
module (F @ noalloc) () = struct
    let bar = allocates2
end
[%%expect{|
val allocates2 : unit -> unit = <fun>
Lines 2-4, characters 21-3:
2 | .....................() = struct
3 |     let bar = allocates2
4 | end
Error: The module is "alloc"
         because it closes over the value "allocates2" at line 3, characters 14-24
         which is "alloc".
       However, the module highlighted is expected to be "noalloc".
|}]

(* CR-soon shsong: error expected -- cannot call [whitewashed ()] *)
let (secretly_allocates @ noalloc) () =
  let (whitewashed @ alloc) = fun () -> { x = 1.; y = 2. } in
  whitewashed ()
[%%expect{|
val secretly_allocates : unit -> t = <fun>
|}]

(* CR-soon shsong: error expected -- cannot assign alloc func to escape_hatch ref *)
let (secretly_allocates' @ noalloc) () = exclave_
 let mutable escape_hatch = None in
 (escape_hatch <- stack_ Some (fun () -> { x = 1.; y = 2. })) [@zero_alloc];
 match escape_hatch with
 | None -> stack_ { x = 1.; y = 2. }
 | Some f -> f ()
[%%expect{|
val secretly_allocates' : unit -> t @ local = <fun>
|}]

(* CR-soon shsong: error expected -- cannot call [f ()] because it is alloc *)
let (secretly_allocates @ noalloc) () = exclave_
 let mutable (escape_hatch @ alloc) = None in
 (escape_hatch <- stack_ Some (fun () -> { x = 1.; y = 2. })) [@zero_alloc];
 match escape_hatch with
 | None -> stack_ { x = 1.; y = 2. }
 | Some f -> f ()
[%%expect{|
val secretly_allocates : unit -> t @ local = <fun>
|}]
