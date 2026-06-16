(* TEST
 flags+="-extension mode_alpha";
 expect;
*)

(* Tests for the Allocation mode axis (modes [alloc] / [noalloc] /
   [noalloc_strict]).

   This file covers the parts of the design doc that are currently implemented:
   the allocation mode as a *type-level mode* on function arguments and return
   values. [alloc] is the legacy/default mode and is therefore not printed.

   The following doc features are NOT implemented yet and are skipped here:
   - [@ noalloc] as a binding/submode constraint that forces a term's mode,
     e.g. [let a_record @ noalloc = ...] or [(e : t @ noalloc)] at a binding
     (doc Examples 1, 3, 4). These currently raise [Typecore.Error].
   - The [@@ noalloc] modality and the [mod noalloc_strict] allocation kind
     (mode crossing): they are accepted but not enforced (a function-containing
     type is wrongly accepted as [mod noalloc_strict]). *)

type t = { x : float; y : float }
[%%expect{|
type t = { x : float; y : float; }
|}]

(* A function that returns a record. The [@ alloc] mode on the
   binding is the legacy default; the [@ noalloc] return mode is part of the
   type and is printed. *)
let (new_record @ alloc) () =
    ({ x = 3.0; y = 4.0 } : t @ noalloc)
[%%expect{|
val new_record : unit -> t @ noalloc = <fun>
|}]

(* The [@ noalloc] return mode can be written directly in the return type. *)
let f () : t @ noalloc = { x = 3.0; y = 4.0 }
[%%expect{|
val f : unit -> t @ noalloc = <fun>
|}]

(* The allocation mode shows up in a [val] signature's function type. *)
module M : sig val f : unit -> t @ noalloc end = struct
    let f () = { x = 3.0; y = 4.0 }
end
[%%expect{|
module M : sig val f : unit -> t @ noalloc end @@ stateless
|}]

(* The allocation mode of a function-typed argument. *)
let apply (g : (unit -> t) @ noalloc) = g
[%%expect{|
val apply : (unit -> t) @ noalloc -> (unit -> t) = <fun>
|}]

(* A [noalloc] return mode propagates through a higher-order function. *)
let call (g : unit -> t @ noalloc) = g ()
[%%expect{|
val call : (unit -> t @ noalloc) -> t @ noalloc = <fun>
|}]


(* Mode capture: a [noalloc] function must not close over [alloc] values.
   These mirror the working [portable]/[nonportable] capture tests, but capture
   enforcement is not implemented for the allocation axis yet, so the cases
   below are wrongly accepted (or crash). The captured output reflects current
   behavior; re-promote once enforcement lands. *)

(* CR-soon shsong: should error -- the closure passed to [noalloc_use] closes
   over [allocates] which is [alloc] but is expected to be [noalloc]. Not
   enforced yet. *)
let noalloc_use : 'a @ noalloc -> unit = fun _ -> ()
let (allocates @ alloc) () = { x = 1.; y = 2. }
let capture_in_closure () = noalloc_use allocates
let capture_in_closure () = noalloc_use (fun () -> allocates ())
[%%expect{|
val noalloc_use : 'a @ noalloc -> unit = <fun>
val allocates : unit -> t = <fun>
val capture_in_closure : unit -> unit = <fun>
val capture_in_closure : unit -> unit = <fun>
|}]

(* CR-soon shsong: should error -- [h] closes over [g] which is [alloc] but is
   ascribed [noalloc]. Not enforced yet. *)
let cap (g @ alloc) =
  let h () = g () in
  (h : _ @ noalloc)
[%%expect{|
val cap : (unit -> 'a) -> (unit -> 'a) @ noalloc = <fun>
|}]

(* CR-soon shsong: should error -- the functor [F] closes over [allocates2]
   which is [alloc] but [F] is [noalloc]. Currently raises an exception. *)
let (allocates2 @ alloc) () = ()
module (F @ noalloc) () = struct
    let bar = allocates2
end
[%%expect{|
val allocates2 : unit -> unit = <fun>
Uncaught exception: Invalid_argument("option is None")

|}]
