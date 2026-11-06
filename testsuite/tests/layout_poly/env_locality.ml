(* TEST
 flags = "-extension layout_poly_alpha";
 expect.opt;
*)

(* Regression tests for the locality of the environment allocation for layout
   polymorphic function. Previously there were because caused by not accounting
   for the locality forced by partial application. *)

external to_float : float# -> float = "%box_float"
let[@inline never] require_global (x @ global) = x
[%%expect{|
external to_float : float# -> float = "%box_float"
val require_global : 'a -> 'a = <fun>
|}]

(* [h 3] is a tail call, so [h] must be global, which forces [f] to be global
   even though it doesn't itself escape. *)
let tail (r : int ref) =
  let poly_ f a b c =
    r := !r + a + b;
    c
  in
  let g = f 1 in
  let h = g 2 in
  h 3

let tail_result =
  let r = ref 0 in
  let x = tail r in
  Gc.full_major ();
  x, !r
[%%expect{|
val tail : int ref -> int = <fun>
val tail_result : int * int = (3, 3)
|}]

(* Without the tail call, [f] and its environment can be local. *)
let non_tail (r : int ref) =
  let poly_ f a b c =
    r := !r + a + b;
    c
  in
  let g = f 1 in
  let h = g 2 in
  let x = h 3 in
  let y = to_float (f 1 2 #4.5) in
  x, y, !r

let non_tail_result = non_tail (ref 0)
[%%expect{|
val non_tail : int ref -> int * float * int = <fun>
val non_tail_result : int * float * int = (3, 4.5, 6)
|}]

(* Capturing a local value forces [f] and its environment to be local. *)
let local_capture (r : int ref @ local) =
  let poly_ f a b c =
    r := !r + a + b;
    c
  in
  let g = f 1 in
  let h = g 2 in
  let x = h 3 in
  let y = to_float (f 1 2 #4.5) in
  x, y, !r

let local_capture_result = local_capture (ref 0)
[%%expect{|
val local_capture : int ref @ local -> int * float * int = <fun>
val local_capture_result : int * float * int = (3, 4.5, 6)
|}]

(* Partial applications can also be forced global without a tail call, for
   example by storing one in a global ref. The environment must outlive
   [stored], so it must be heap-allocated. *)
let global_ref : (int -> int) ref = ref (fun x -> x)

let stored (r : int ref) =
  let poly_ f a b =
    r := !r + a;
    b
  in
  global_ref := f 1

let stored_result =
  let r = ref 0 in
  stored r;
  Gc.full_major ();
  let x = !global_ref 5 in
  x, !r
[%%expect{|
val global_ref : (int -> int) ref = {contents = <fun>}
val stored : int ref -> unit = <fun>
val stored_result : int * int = (5, 1)
|}]

(* Or by passing one as an argument that must be global. *)
let passed (r : int ref) =
  let poly_ f a b =
    r := !r + a;
    b
  in
  require_global (f 1) 2

let passed_result =
  let r = ref 0 in
  let x = passed r in
  x, !r
[%%expect{|
val passed : int ref -> int = <fun>
val passed_result : int * int = (2, 1)
|}]

(* A local parameter gives [f] local partial applications, but the tail call
   still forces [g] to be global, so [f] and its environment are
   heap-allocated. *)
let local_param (r : int ref) =
  let poly_ f a (b @ local) c =
    r := !r + a + !b;
    c
  in
  let g = f 1 in
  g (ref 2) 3

let local_param_result =
  let r = ref 0 in
  let x = local_param r in
  x, !r
[%%expect{|
val local_param : int ref -> int = <fun>
val local_param_result : int * int = (3, 3)
|}]
