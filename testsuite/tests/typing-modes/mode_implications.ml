(* TEST
 flags = "-w -220";
 expect;
*)

external use_local : 'a @ local -> unit = "%ignore"

external requires_unyielding : 'a @ local unyielding -> unit
  = "%ignore"

external requires_uncontended : 'a @ uncontended immutable -> unit
  = "%ignore"
external requires_portable : 'a @ portable -> unit = "%ignore"

module type Runnable = sig val run : unit -> unit end
[%%expect{|
external use_local : 'a @ local -> unit = "%ignore"
external requires_unyielding : 'a @ local unyielding -> unit = "%ignore"
external requires_uncontended : 'a @ immutable uncontended -> unit
  = "%ignore"
external requires_portable : 'a @ portable -> unit = "%ignore"
module type Runnable = sig val run : unit -> unit end
|}]

(* Part 1: fixed mode positions. *)

let arrow_arg_local : (unit -> unit) @ local -> unit =
  fun g -> requires_unyielding g
[%%expect{|
Line 2, characters 31-32:
2 |   fun g -> requires_unyielding g
                                   ^
Error: This value is "yielding" but is expected to be "unyielding".
|}]

module Arrow_ret_local : sig
  val f : int -> (unit -> int) @ local
end = struct
  let f x = fun () -> x
end
[%%expect{|
module Arrow_ret_local : sig val f : int -> (unit -> int) @ local end
|}]

let use_arrow_ret_local () =
  let r = Arrow_ret_local.f 3 in
  requires_unyielding r
[%%expect{|
Line 3, characters 22-23:
3 |   requires_unyielding r
                          ^
Error: This value is "yielding" but is expected to be "unyielding".
|}]

module Arrow_eq : sig
  val f : 'a @ local yielding -> unit
end = struct
  let f : 'a @ local -> unit = fun _ -> ()
end
[%%expect{|
module Arrow_eq : sig val f : 'a @ local -> unit end
|}]

let _ = (requires_unyielding : (string -> unit) @ local -> unit)
[%%expect{|
Line 1, characters 9-28:
1 | let _ = (requires_unyielding : (string -> unit) @ local -> unit)
             ^^^^^^^^^^^^^^^^^^^
Error: The value "requires_unyielding" has type "'a @ local unyielding -> unit"
       but an expression was expected of type
         "(string -> unit) @ local -> unit"
|}]

let storage = ref ""

let with_effect : ((string -> unit) @ local yielding -> 'a) -> 'a =
  fun f -> f ((:=) storage)

let run_default : (string -> unit) @ local -> unit =
  fun f -> f "some string"

let () = with_effect (fun k -> run_default k)

let _ = !storage
[%%expect{|
val storage : string ref = {contents = ""}
val with_effect : ((string -> unit) @ local -> 'a) -> 'a = <fun>
val run_default : (string -> unit) @ local -> unit = <fun>
- : string = "some string"
|}]

let arrow_arg_read : 'a ref @ read -> unit =
  fun x -> requires_uncontended x
[%%expect{|
Line 2, characters 32-33:
2 |   fun x -> requires_uncontended x
                                    ^
Error: This value is "shared" but is expected to be "uncontended".
|}]

module Arrow_ret_read : sig
  val peek : 'a ref -> 'a @ read
end = struct
  let peek r = r.contents
end
[%%expect{|
module Arrow_ret_read : sig val peek : 'a ref -> 'a @ read end
|}]

let () = requires_uncontended (Arrow_ret_read.peek (ref (ref 0)))
[%%expect{|
Line 1, characters 30-65:
1 | let () = requires_uncontended (Arrow_ret_read.peek (ref (ref 0)))
                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "shared" but is expected to be "uncontended".
|}]

let arrow_arg_reading : (unit -> unit) @ reading -> unit =
  fun f -> requires_portable f
[%%expect{|
Line 2, characters 29-30:
2 |   fun f -> requires_portable f
                                 ^
Error: This value is "shareable" but is expected to be "portable".
|}]

let take_reading : (unit -> unit) @ reading -> unit = fun f -> f ()

let pass_nonportable_to_reading () =
  let np @ nonportable = fun () -> () in
  take_reading np
[%%expect{|
val take_reading : (unit -> unit) @ reading -> unit = <fun>
Line 5, characters 15-17:
5 |   take_reading np
                   ^^
Error: This value is "nonportable" but is expected to be "shareable".
|}]

module rec Rec_annotated : Runnable @ stateless = struct
  let run () = ()
end

let () = requires_portable (module Rec_annotated : Runnable)
[%%expect{|
module rec Rec_annotated : Runnable
|}]

module rec Rec_plain : Runnable = struct
  let run () = ()
end

let () = requires_portable (module Rec_plain : Runnable)
[%%expect{|
module rec Rec_plain : Runnable
Line 5, characters 27-56:
5 | let () = requires_portable (module Rec_plain : Runnable)
                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* Part 2: flexible sites. *)

let param_local_unconstrained (x @ local) = ()
[%%expect{|
val param_local_unconstrained : 'a @ local -> unit = <fun>
|}]

let param_local_unyielding (x @ local) = requires_unyielding x
[%%expect{|
Line 1, characters 61-62:
1 | let param_local_unyielding (x @ local) = requires_unyielding x
                                                                 ^
Error: This value is "yielding" but is expected to be "unyielding".
|}]

let param_explicit_yielding (x @ local yielding) = requires_unyielding x
[%%expect{|
Line 1, characters 71-72:
1 | let param_explicit_yielding (x @ local yielding) = requires_unyielding x
                                                                           ^
Error: This value is "yielding" but is expected to be "unyielding".
|}]

let let_local_unyielding () =
  let g @ local = fun () -> () in
  requires_unyielding g
[%%expect{|
Line 3, characters 22-23:
3 |   requires_unyielding g
                          ^
Error: This value is "yielding" but is expected to be "unyielding".
|}]

let let_local_still_escapes () =
  let g @ local = fun () -> () in
  g
[%%expect{|
Line 3, characters 2-3:
3 |   g
      ^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

let constraint_mode_only () =
  let g = fun () -> () in
  requires_unyielding (g : _ @ local)
[%%expect{|
Line 3, characters 22-37:
3 |   requires_unyielding (g : _ @ local)
                          ^^^^^^^^^^^^^^^
Error: This value is "yielding" but is expected to be "unyielding".
|}]

let constraint_type_and_mode () =
  let g = fun () -> () in
  requires_unyielding (g : (unit -> unit) @ local)
[%%expect{|
Line 3, characters 22-50:
3 |   requires_unyielding (g : (unit -> unit) @ local)
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "yielding" but is expected to be "unyielding".
|}]

let constraint_type_only () =
  let g = fun () -> () in
  requires_unyielding (g : unit -> unit)
[%%expect{|
val constraint_type_only : unit -> unit = <fun>
|}]

let param_type_and_mode (x : ('a -> 'a) @ local) = requires_unyielding x
[%%expect{|
Line 1, characters 71-72:
1 | let param_type_and_mode (x : ('a -> 'a) @ local) = requires_unyielding x
                                                                           ^
Error: This value is "yielding" but is expected to be "unyielding".
|}]

let param_read_unconstrained (_x @ read) = ()
[%%expect{|
val param_read_unconstrained : 'a @ read -> unit = <fun>
|}]

let param_read_uncontended (x @ read) = requires_uncontended x
[%%expect{|
Line 1, characters 61-62:
1 | let param_read_uncontended (x @ read) = requires_uncontended x
                                                                 ^
Error: This value is "shared" but is expected to be "uncontended".
|}]

let param_reading_unconstrained (f @ reading) = f ()
[%%expect{|
val param_reading_unconstrained : (unit -> 'a) @ reading -> 'a = <fun>
|}]

let param_reading_portable (f @ reading) = requires_portable f
[%%expect{|
Line 1, characters 61-62:
1 | let param_reading_portable (f @ reading) = requires_portable f
                                                                 ^
Error: This value is "shareable" but is expected to be "portable".
|}]

let ret_local_unconstrained x : _ @ local = fun () -> x
[%%expect{|
val ret_local_unconstrained : 'a -> (unit -> 'a) @ local = <fun>
|}]

module Ret_local_constrained = struct
  let f x : _ @ local = fun () -> x

  let use () =
    let r = f 3 in
    requires_unyielding r
end
[%%expect{|
Line 6, characters 24-25:
6 |     requires_unyielding r
                            ^
Error: This value is "yielding" but is expected to be "unyielding".
|}]

let get (x @ read uncontended) = x.contents
[%%expect{|
val get : 'a ref @ read uncontended -> 'a @ read uncontended = <fun>
|}]

let () = requires_uncontended (get { contents = ref 0 })
[%%expect{|
|}]

let ret_read (x @ read uncontended) : _ @ read = x.contents
[%%expect{|
val ret_read : 'a ref @ read uncontended -> 'a @ read = <fun>
|}]

let () = requires_uncontended (ret_read { contents = ref 0 })
[%%expect{|
Line 1, characters 30-61:
1 | let () = requires_uncontended (ret_read { contents = ref 0 })
                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "shared" but is expected to be "uncontended".
|}]

let ret_write (x @ write uncontended) : _ @ write = x
[%%expect{|
val ret_write : 'a @ write uncontended -> 'a @ write = <fun>
|}]

let infer_arg_visibility x = param_read_unconstrained x
[%%expect{|
val infer_arg_visibility : 'a -> unit = <fun>
|}]

let call_at_read (y @ read) = infer_arg_visibility y
[%%expect{|
Line 1, characters 51-52:
1 | let call_at_read (y @ read) = infer_arg_visibility y
                                                       ^
Error: This value is "read" but is expected to be "read_write".
|}]

module Sig_forces_unyielding : sig
  val f : 'a @ local unyielding -> 'b @ local unyielding -> unit
end = struct
  let f (x @ local) (y @ local) = use_local x; use_local y
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x @ local) (y @ local) = use_local x; use_local y
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a @ local -> 'b @ local -> unit end
       is not included in
         sig
           val f : 'a @ local unyielding -> 'b @ local unyielding -> unit
         end
       Values do not match:
         val f : 'a @ local -> 'b @ local -> unit
       is not included in
         val f : 'a @ local unyielding -> 'b @ local unyielding -> unit
       The type "'a @ local -> 'b @ local -> unit"
       is not compatible with the type
         "'a @ local unyielding -> 'b @ local unyielding -> unit"
|}]

let coerce_stateless (g @ stateless nonportable) =
  let module M =
    (struct let run () = g () end : Runnable @ stateless)
  in
  M.run ()
[%%expect{|
Line 3, characters 5-33:
3 |     (struct let run () = g () end : Runnable @ stateless)
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig val run : unit -> '_weak1 end @ nonportable
       is not included in
         Runnable @ portable
       Values do not match:
         val run : unit -> '_weak1 (* in a structure at nonportable *)
       is not included in
         val run : unit -> unit (* in a structure at portable *)
       The first is "nonportable"
         because it closes over the value "g" at line 3, characters 25-26
         which is "nonportable".
       However, the second is "portable".
|}]

let annotate_stateless (g @ stateless nonportable) =
  let module M = (struct let run () = g () end @ stateless) in
  M.run ()
[%%expect{|
Line 2, characters 18-46:
2 |   let module M = (struct let run () = g () end @ stateless) in
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The module is "nonportable"
         because it contains the value "run" defined as the expression at line 2, characters 29-32
         which is "nonportable"
         because it closes over the value "g" at line 2, characters 38-39
         which is "nonportable".
       However, the module highlighted is expected to be "portable".
|}]

let coerce_explicit (g @ stateless nonportable) =
  let module M =
    (struct let run () = g () end : Runnable @ stateless nonportable)
  in
  M.run ()
[%%expect{|
val coerce_explicit : (unit -> unit) @ stateless nonportable -> unit = <fun>
|}]

let ho_yielding (g : _ @ local unyielding -> unit) =
  fun (x @ local) -> g x
[%%expect{|
Line 2, characters 23-24:
2 |   fun (x @ local) -> g x
                           ^
Error: This value is "yielding" but is expected to be "unyielding".
|}]

let ho_uncontended (g : _ @ uncontended immutable -> unit) =
  fun (x @ read) -> g x
[%%expect{|
Line 2, characters 22-23:
2 |   fun (x @ read) -> g x
                          ^
Error: This value is "shared" but is expected to be "uncontended".
|}]

let ho_pass (x @ local) f = f x
[%%expect{|
val ho_pass : 'a @ local -> ('a @ local -> 'b) -> 'b = <fun>
|}]

let curried (x @ local) (y @ local) =
  requires_unyielding y;
  use_local x
[%%expect{|
Line 2, characters 22-23:
2 |   requires_unyielding y;
                          ^
Error: This value is "yielding" but is expected to be "unyielding".
|}]
