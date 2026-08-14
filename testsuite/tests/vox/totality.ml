(* TEST
 expect;
*)

(* Tests for the totality and logicality mode axes.

   Totality (comonadic): total < partial, legacy partial.
   Logicality (monadic): physical < logical, legacy physical.
   The two are a comonadic/monadic pair like portability and contention. *)

(* ------------------------------------------------------------------ *)
(* Submoding both ways on each axis. *)

let f (g @ total) = g 0
let g_total @ total = fun x -> x + 1
let _ = f g_total (* total accepted where partial is expected, via [f]'s use *)
[%%expect{|
val f : (int -> 'a) @ total -> 'a = <fun>
val g_total : int -> int = <fun>
- : int = 1
|}]

(* partial rejected where total is expected *)
let g_partial = fun x -> x + 1
let _ = f g_partial
[%%expect{|
val g_partial : int -> int = <fun>
Line 2, characters 10-19:
2 | let _ = f g_partial
              ^^^^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

(* physical accepted where logical is expected *)
let uses_logical (_x @ logical) = ()
let phys = 42
let _ = uses_logical phys
[%%expect{|
val uses_logical : 'a @ logical -> unit = <fun>
val phys : int = 42
- : unit = ()
|}]

(* logical rejected where physical is expected, on a type that does not
   cross logicality *)
let wants_physical (x @ physical) = ignore x
let use_logical_ref (r @ logical) = wants_physical r
[%%expect{|
val wants_physical : 'a -> unit = <fun>
Line 2, characters 51-52:
2 | let use_logical_ref (r @ logical) = wants_physical r
                                                       ^
Error: This value is "logical" but is expected to be "physical".
|}]

(* ------------------------------------------------------------------ *)
(* Defaults: an unannotated value is partial and physical, and prints as it
   does today. *)

let unannotated = fun x -> x
[%%expect{|
val unannotated : 'a -> 'a = <fun>
|}]

(* ------------------------------------------------------------------ *)
(* Crossing positives: a logical int and a logical arrow used at physical. *)

let cross_int (x : int @ logical) = wants_physical x
let cross_arrow (f : (int -> int) @ logical) = wants_physical f
[%%expect{|
val cross_int : int @ logical -> unit = <fun>
val cross_arrow : (int -> int) @ logical -> unit = <fun>
|}]

(* Crossing negatives: an int ref and an Atomic.t stay logical. *)

let no_cross_ref (r : int ref @ logical) = wants_physical r
[%%expect{|
Line 1, characters 58-59:
1 | let no_cross_ref (r : int ref @ logical) = wants_physical r
                                                              ^
Error: This value is "logical" but is expected to be "physical".
|}]

let no_cross_atomic (a : int Atomic.t @ logical) = wants_physical a
[%%expect{|
Line 1, characters 66-67:
1 | let no_cross_atomic (a : int Atomic.t @ logical) = wants_physical a
                                                                      ^
Error: This value is "logical" but is expected to be "physical".
|}]

(* A type crosses totality when it has no functions. *)
let cross_totality_int (x : int) = f (fun _ -> x)
[%%expect{|
val cross_totality_int : int -> int = <fun>
|}]

(* ------------------------------------------------------------------ *)
(* Modality position. *)

module M : sig
  val empty : int list @@ total
  val logical_int : int @@ logical
end = struct
  let empty @ total = []
  let logical_int @ logical = 42
end
[%%expect{|
module M :
  sig val empty : int list @@ total val logical_int : int @@ logical end
|}]

(* [@@ physical] does not exist; the Physical end is spelled [nonlogical]. *)
module type S = sig
  val x : int @@ physical
end
[%%expect{|
Line 2, characters 17-25:
2 |   val x : int @@ physical
                     ^^^^^^^^
Error: Unrecognized modality physical.
|}]

module type S = sig
  val x : int ref @@ nonlogical
end
[%%expect{|
Line 2, characters 21-31:
2 |   val x : int ref @@ nonlogical
                         ^^^^^^^^^^
Warning 220 [redundant-modality]: This modality is redundant.

module type S = sig val x : int ref end
|}]

(* ------------------------------------------------------------------ *)
(* Kind modifiers. *)

type t_total : value mod total
type t_logical : value mod logical
[%%expect{|
type t_total : value mod total
type t_logical : value mod logical
|}]

(* ------------------------------------------------------------------ *)
(* The allowlist: primitives on the allowlist are total. *)

let increment @ total = fun x -> x + 1
[%%expect{|
val increment : int -> int = <fun>
|}]

(* A closure calling a partial function is rejected at [@ total]. *)
let calls_partial @ total = fun () -> g_partial 0
[%%expect{|
Line 1, characters 38-47:
1 | let calls_partial @ total = fun () -> g_partial 0
                                          ^^^^^^^^^
Error: The value "g_partial" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 28-49
         which is expected to be "total".
|}]

(* ------------------------------------------------------------------ *)
(* The capture bump: the pair working together. *)

(* A total closure capturing a partial value: rejected.  The parameter must
   be pinned partial, otherwise inference strengthens it to total. *)
let capture_partial (h @ partial) =
  f (fun x -> h (); x)
[%%expect{|
Line 2, characters 14-15:
2 |   f (fun x -> h (); x)
                  ^
Error: The value "h" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 2, characters 4-22
         which is expected to be "total".
|}]

(* A total closure capturing an int ref and reading it: the capture arrives
   logical, and the read needs physical.  The read is a direct mutable-field
   projection; going through [!] would instead report [!] itself as partial. *)
let capture_ref (r : int ref) =
  f (fun _ -> r.contents)
[%%expect{|
Line 2, characters 14-15:
2 |   f (fun _ -> r.contents)
                  ^
Error: This value is "logical"
         because it is used inside the function at line 2, characters 4-25
         which is expected to be "total".
       However, the highlighted expression is expected to be "physical"
         because its mutable field "contents" is being read.
|}]

(* The same closure capturing an int: accepted, because int crosses
   logicality. *)
let capture_int (n : int) =
  f (fun _ -> n)
[%%expect{|
val capture_int : int -> int = <fun>
|}]

(* The portability/contention twin behaves identically on the same fixture:
   a portable closure capturing an int ref and reading it is rejected because
   the capture arrives contended. *)
let wants_portable (g : (int -> int) @ portable) = g 0
let capture_ref_portable (r : int ref) =
  wants_portable (fun _ -> r.contents)
[%%expect{|
val wants_portable : (int -> int) @ portable -> int = <fun>
Line 3, characters 27-28:
3 |   wants_portable (fun _ -> r.contents)
                               ^
Error: This value is "contended"
         because it is used inside the function at line 3, characters 17-38
         which is expected to be "portable".
       However, the highlighted expression is expected to be "shared" or "uncontended"
         because its mutable field "contents" is being read.
|}]

(* Reading or writing a mutable field through a logical value is rejected,
   much as through a contended value. *)
let read_logical (r @ logical) = (r : int ref).contents
[%%expect{|
Line 1, characters 34-35:
1 | let read_logical (r @ logical) = (r : int ref).contents
                                      ^
Error: This value is "logical"
       but is expected to be "physical"
         because its mutable field "contents" is being read.
|}]

let write_logical (r @ logical) = (r : int ref).contents <- 1
[%%expect{|
Line 1, characters 35-36:
1 | let write_logical (r @ logical) = (r : int ref).contents <- 1
                                       ^
Error: This value is "logical"
       but is expected to be "physical"
         because its mutable field "contents" is being written.
|}]

(* ------------------------------------------------------------------ *)
(* Effects: each rejected at [@ total], and accepted without the
   annotation. *)

let uses_ref @ total = fun () -> ref 0
[%%expect{|
Line 1, characters 33-36:
1 | let uses_ref @ total = fun () -> ref 0
                                     ^^^
Error: The value "ref" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 23-38
         which is expected to be "total".
|}]
let uses_ref = fun () -> ref 0
[%%expect{|
val uses_ref : unit -> int ref = <fun>
|}]

let uses_deref @ total = fun (r : int ref) -> !r
[%%expect{|
Line 1, characters 46-47:
1 | let uses_deref @ total = fun (r : int ref) -> !r
                                                  ^
Error: The value "(!)" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 25-48
         which is expected to be "total".
|}]
let uses_deref = fun (r : int ref) -> !r
[%%expect{|
val uses_deref : int ref -> int = <fun>
|}]

let uses_assign @ total = fun (r : int ref) -> r := 1
[%%expect{|
Line 1, characters 49-51:
1 | let uses_assign @ total = fun (r : int ref) -> r := 1
                                                     ^^
Error: The value "(:=)" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 26-53
         which is expected to be "total".
|}]
let uses_assign = fun (r : int ref) -> r := 1
[%%expect{|
val uses_assign : int ref -> unit = <fun>
|}]

let uses_while @ total = fun () -> while false do () done
[%%expect{|
Line 1, characters 35-57:
1 | let uses_while @ total = fun () -> while false do () done
                                       ^^^^^^^^^^^^^^^^^^^^^^
Error: The function is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 25-57
         which is expected to be "total".
|}]
let uses_while = fun () -> while false do () done
[%%expect{|
val uses_while : unit -> unit = <fun>
|}]

let uses_for @ total = fun () -> for _i = 0 to 10 do () done
[%%expect{|
Line 1, characters 33-60:
1 | let uses_for @ total = fun () -> for _i = 0 to 10 do () done
                                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The function is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 23-60
         which is expected to be "total".
|}]
let uses_for = fun () -> for _i = 0 to 10 do () done
[%%expect{|
val uses_for : unit -> unit = <fun>
|}]

type mrec = { mutable x : int }
let uses_setfield @ total = fun (r : mrec) -> r.x <- 1
[%%expect{|
type mrec = { mutable x : int; }
Line 2, characters 46-54:
2 | let uses_setfield @ total = fun (r : mrec) -> r.x <- 1
                                                  ^^^^^^^^
Error: The function is "partial"
       but is expected to be "total"
         because it is used inside the function at line 2, characters 28-54
         which is expected to be "total".
|}]
let uses_setfield = fun (r : mrec) -> r.x <- 1
[%%expect{|
val uses_setfield : mrec -> unit = <fun>
|}]

let uses_array_set @ total = fun (a : int array) -> a.(0) <- 1
[%%expect{|
Line 1, characters 52-62:
1 | let uses_array_set @ total = fun (a : int array) -> a.(0) <- 1
                                                        ^^^^^^^^^^
Error: The value "Array.set" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 29-62
         which is expected to be "total".
|}]
let uses_array_set = fun (a : int array) -> a.(0) <- 1
[%%expect{|
val uses_array_set : int array -> unit = <fun>
|}]

(* A closure capturing a partial value from an outer scope. *)
let outer_partial (h @ partial) @ total = fun () -> fun () -> h ()
[%%expect{|
Line 1, characters 62-63:
1 | let outer_partial (h @ partial) @ total = fun () -> fun () -> h ()
                                                                  ^
Error: The value "h" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 42-66
         which is expected to be "total".
|}]

let uses_assert @ total = fun x -> assert (x > 0)
[%%expect{|
Line 1, characters 35-49:
1 | let uses_assert @ total = fun x -> assert (x > 0)
                                       ^^^^^^^^^^^^^^
Error: The function is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 26-49
         which is expected to be "total".
|}]
let uses_assert = fun x -> assert (x > 0)
[%%expect{|
val uses_assert : int -> unit = <fun>
|}]

(* A non-exhaustive match can raise [Match_failure]. *)
let partial_match @ total = fun (x : int option) -> match x with Some y -> y
[%%expect{|
Line 1, characters 52-76:
1 | let partial_match @ total = fun (x : int option) -> match x with Some y -> y
                                                        ^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "None"

Line 1, characters 52-76:
1 | let partial_match @ total = fun (x : int option) -> match x with Some y -> y
                                                        ^^^^^^^^^^^^^^^^^^^^^^^^
Error: The function is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 28-76
         which is expected to be "total".
|}]
let exhaustive_match @ total = fun (x : int option) ->
  match x with Some y -> y | None -> 0
[%%expect{|
val exhaustive_match : int option -> int = <fun>
|}]

(* ------------------------------------------------------------------ *)
(* Totality is capture-based: parameters are not captures.  A total closure
   may call a partial parameter, read mutable state through a parameter, or
   send a message to a parameter object.  Termination of calls from the total
   fragment still follows compositionally, because a total context can only
   supply total (and logical) arguments.  The portability/contention twin
   behaves identically on all three shapes. *)

let calls_partial_param @ total = fun (h @ partial) -> h ()
let calls_nonportable_param @ portable = fun (h @ nonportable) -> h ()
[%%expect{|
val calls_partial_param : (unit -> 'a) -> 'a = <fun>
val calls_nonportable_param : (unit -> 'a) -> 'a = <fun>
|}]

let reads_param @ total = fun (r : int ref) -> r.contents
[%%expect{|
val reads_param : int ref -> int = <fun>
|}]

(* ... but a total context cannot supply the partial argument such a function
   would need to go wrong. *)
let _ @ total = fun () -> calls_partial_param g_partial
[%%expect{|
Line 1, characters 46-55:
1 | let _ @ total = fun () -> calls_partial_param g_partial
                                                  ^^^^^^^^^
Error: The value "g_partial" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 16-55
         which is expected to be "total".
|}]

(* Instance-variable assignment constrains enclosing closures. *)
class has_ivar = object
  val mutable v = 0
  method set_via_total = let f @ total = fun () -> v <- 1 in f ()
end
[%%expect{|
Line 3, characters 51-57:
3 |   method set_via_total = let f @ total = fun () -> v <- 1 in f ()
                                                       ^^^^^^
Error: The function is "partial"
       but is expected to be "total"
         because it is used inside the function at line 3, characters 41-57
         which is expected to be "total".
|}]

(* [try] itself is not an effect form: catching does not diverge, and a body
   that could raise is already partial. *)
exception E
let uses_try @ total = fun () -> try 1 with E -> 2
[%%expect{|
exception E
val uses_try : unit -> int = <fun>
|}]

(* ------------------------------------------------------------------ *)
(* Recursion. *)

let rec looper x = looper x
let _ = f looper
[%%expect{|
val looper : 'a -> 'b = <fun>
Line 2, characters 10-16:
2 | let _ = f looper
              ^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

(* The boundary case: a top-level recursive function captured into a total
   closure. *)
let capture_rec @ total = fun x -> looper x
[%%expect{|
Line 1, characters 35-41:
1 | let capture_rec @ total = fun x -> looper x
                                       ^^^^^^
Error: The value "looper" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 26-43
         which is expected to be "total".
|}]

(* The hereditary case: the same [let rec] written locally inside a total
   closure, where the self-reference never crosses a capture boundary. *)
let local_rec @ total = fun () ->
  let rec go x = go x in
  go 0
[%%expect{|
Line 2, characters 17-19:
2 |   let rec go x = go x in
                     ^^
Error: The value "go" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 2, characters 13-21
         which is expected to be "total"
         because it is used inside the function at lines 1-3, characters 24-6
         which is expected to be "total".
|}]

(* A let-bound function literal inside a total closure, which the
   expected-mode edge does not reach: the literal itself is fine (and total),
   so this is accepted. *)
let let_bound_literal @ total = fun () ->
  let id = fun x -> x in
  id 0
[%%expect{|
val let_bound_literal : unit -> int = <fun>
|}]

(* ... but a let-bound literal with a partial body is rejected. *)
let let_bound_partial @ total = fun () ->
  let bad = fun () -> ref 0 in
  bad
[%%expect{|
Line 2, characters 22-25:
2 |   let bad = fun () -> ref 0 in
                          ^^^
Error: The value "ref" is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 1-3, characters 32-5
         which is expected to be "total".
|}]

(* The whole recursive group sits at partial, including a member that never
   refers to the group: the group shares one mode variable. *)
let rec rec_member x = rec_member x
and plain_member x = x
let _ = f plain_member
[%%expect{|
val rec_member : 'a -> 'b = <fun>
val plain_member : 'a -> 'a = <fun>
Line 3, characters 10-22:
3 | let _ = f plain_member
              ^^^^^^^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

(* Mutual recursion.  Written with [match] rather than [=] so that the only
   partiality source is the recursion itself. *)
let mutual @ total = fun () ->
  let rec even x = match x with 0 -> true | n -> odd (n - 1)
  and odd x = match x with 0 -> false | n -> even (n - 1) in
  even 10
[%%expect{|
Line 2, characters 49-52:
2 |   let rec even x = match x with 0 -> true | n -> odd (n - 1)
                                                     ^^^
Error: The value "odd" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 2, characters 15-60
         which is expected to be "total"
         because it is used inside the function at lines 1-4, characters 21-9
         which is expected to be "total".
|}]

(* A recursive function that is only returned, never applied, still makes the
   closure partial: the closure returns a partial value it captured. *)
let returns_rec @ total = fun () -> looper
[%%expect{|
Line 1, characters 36-42:
1 | let returns_rec @ total = fun () -> looper
                                        ^^^^^^
Error: The value "looper" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 26-42
         which is expected to be "total".
|}]

(* KNOWN GAP (see the design doc's decision log): a recursive module's
   signature can claim [@@ total] and the claim justifies its own recursive
   call, because the body is checked against the declared signature.
   Termination is inductive, so this self-assumption is unsound; fixing it
   needs the recursive-approximation environment to weaken totality
   modalities, a follow-up. This fixture pins the current behaviour so the
   fix shows up as a diff here. *)
module rec MRec : sig val loop : int -> int @@ total end = struct
  let loop x = MRec.loop x
end
let uses_rec_module @ total = fun () -> MRec.loop 0
[%%expect{|
module rec MRec : sig val loop : int -> int @@ total end
val uses_rec_module : unit -> int = <fun>
|}]

(* Without the modality claim, the recursive module's value stays partial. *)
module rec NRec : sig val loop : int -> int end = struct
  let loop x = NRec.loop x
end
let uses_rec_module2 @ total = fun () -> NRec.loop 0
[%%expect{|
module rec NRec : sig val loop : int -> int end
Line 4, characters 41-50:
4 | let uses_rec_module2 @ total = fun () -> NRec.loop 0
                                             ^^^^^^^^^
Error: The value "NRec.loop" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 4, characters 31-52
         which is expected to be "total".
|}]

(* An arrow-free recursive value crosses totality at its use sites. *)
let rec one = 1
let use_one @ total = fun () -> one
[%%expect{|
val one : int = 1
val use_one : unit -> int = <fun>
|}]
