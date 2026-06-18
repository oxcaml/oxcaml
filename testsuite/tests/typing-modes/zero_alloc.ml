(* TEST
 flags+="-extension mode_alpha";
 expect;
*)


(** Test 1: axis order noalloc_strict < noalloc < alloc *)

(* A plain abstract type has the default kind and so does NOT cross the
   allocation axis (unlike the [cross_*] types below). Its allocation
   mode is therefore observable, which lets us check submoding: a value
   at mode [m] can be used where [m'] is expected iff [m <= m'], with
   [noalloc_strict < noalloc < alloc]. We cover all 6 ordered pairs. *)
type t
[%%expect{|
type t
|}]

(* noalloc_strict <= noalloc: ok *)
let order_ss_n (x : t @ noalloc_strict) : _ @ noalloc = x
[%%expect{|
val order_ss_n : t @ noalloc_strict -> t @ noalloc = <fun>
|}]

(* noalloc_strict <= alloc: ok *)
let order_ss_a (x : t @ noalloc_strict) : _ @ alloc = x
[%%expect{|
val order_ss_a : t @ noalloc_strict -> t = <fun>
|}]

(* noalloc <= alloc: ok *)
let order_n_a (x : t @ noalloc) : _ @ alloc = x
[%%expect{|
val order_n_a : t @ noalloc -> t = <fun>
|}]

(* noalloc > noalloc_strict: error *)
let order_n_ss (x : t @ noalloc) : _ @ noalloc_strict = x
[%%expect{|
Line 1, characters 56-57:
1 | let order_n_ss (x : t @ noalloc) : _ @ noalloc_strict = x
                                                            ^
Error: This value is "noalloc" but is expected to be "noalloc_strict".
|}]

(* alloc > noalloc: error *)
let order_a_n (x : t @ alloc) : _ @ noalloc = x
[%%expect{|
Line 1, characters 46-47:
1 | let order_a_n (x : t @ alloc) : _ @ noalloc = x
                                                  ^
Error: This value is "alloc" but is expected to be "noalloc".
|}]

(* alloc > noalloc_strict: error *)
let order_a_ss (x : t @ alloc) : _ @ noalloc_strict = x
[%%expect{|
Line 1, characters 54-55:
1 | let order_a_ss (x : t @ alloc) : _ @ noalloc_strict = x
                                                          ^
Error: This value is "alloc" but is expected to be "noalloc_strict".
|}]


(** Test 2: captures for axis order noalloc_strict < noalloc < alloc *)

(* When a closure captures a value, the closure inherits that value's
   allocation mode (the closure is at least as [alloc] as everything it
   closes over, cf. the [cap] example above). So closing over a value at
   mode [m] and then requiring the closure at [m'] succeeds iff
   [m <= m'], with [noalloc_strict < noalloc < alloc]. We cover all 6
   ordered pairs, reusing the non-crossing abstract type [t]. *)
type t
[%%expect{|
type t
|}]

(* capture noalloc_strict, require noalloc: ok *)
let cap_ss_n (g : t @ noalloc_strict) =
  let h () = g in
  (h : _ @ noalloc)
[%%expect{|
val cap_ss_n : t @ noalloc_strict -> unit -> t = <fun>
|}]

(* capture noalloc_strict, require alloc: ok *)
let cap_ss_a (g : t @ noalloc_strict) =
  let h () = g in
  (h : _ @ alloc)
[%%expect{|
val cap_ss_a : t @ noalloc_strict -> unit -> t = <fun>
|}]

(* capture noalloc, require alloc: ok *)
let cap_n_a (g : t @ noalloc) =
  let h () = g in
  (h : _ @ alloc)
[%%expect{|
val cap_n_a : t @ noalloc -> unit -> t = <fun>
|}]

(* capture noalloc, require noalloc_strict: error *)
let cap_n_ss (g : t @ noalloc) =
  let h () = g in
  (h : _ @ noalloc_strict)
[%%expect{|
Line 3, characters 3-4:
3 |   (h : _ @ noalloc_strict)
       ^
Error: This value is "noalloc"
         because it closes over the value "g" at line 2, characters 13-14
         which is "noalloc".
       However, the highlighted expression is expected to be "noalloc_strict".
|}]

(* capture alloc, require noalloc: error *)
let cap_a_n (g : t @ alloc) =
  let h () = g in
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

(* capture alloc, require noalloc_strict: error *)
let cap_a_ss (g : t @ alloc) =
  let h () = g in
  (h : _ @ noalloc_strict)
[%%expect{|
Line 3, characters 3-4:
3 |   (h : _ @ noalloc_strict)
       ^
Error: This value is "alloc"
         because it closes over the value "g" at line 2, characters 13-14
         which is "alloc".
       However, the highlighted expression is expected to be "noalloc_strict".
|}]


(** Test 3: modalities @@ noalloc and @@ noalloc_strict *)

(* A field modality caps the field's allocation mode independently of the
   record's own mode. This mirrors the [@@ global] modality tests for
   locality in [testsuite/tests/typing-modes/global_and_aliased.ml] and
   the [val] modality tests in
   [testsuite/tests/typing-modes/incl_modalities.ml]. *)
type 'a noalloc_field = { fn : 'a @@ noalloc }
type 'a noalloc_strict_field = { fss : 'a @@ noalloc_strict }
[%%expect{|
type 'a noalloc_field = { fn : 'a @@ noalloc; }
type 'a noalloc_strict_field = { fss : 'a @@ noalloc_strict; }
|}]

(* Projection: even from an [alloc] (default) record, the field comes out
   capped at its modality. *)
let proj_n ({ fn } : 'a noalloc_field @ alloc) : 'a @ noalloc = fn
[%%expect{|
val proj_n : 'a noalloc_field -> 'a @ noalloc = <fun>
|}]

let proj_ss ({ fss } : 'a noalloc_strict_field @ alloc) : 'a @ noalloc = fss
[%%expect{|
val proj_ss : 'a noalloc_strict_field -> 'a @ noalloc = <fun>
|}]

let proj_ss ({ fss } : 'a noalloc_strict_field @ noalloc) : 'a @ noalloc_strict = fss
[%%expect{|
val proj_ss : 'a noalloc_strict_field @ noalloc -> 'a @ noalloc_strict =
  <fun>
|}]

let proj_ss ({ fss } : 'a noalloc_strict_field @ alloc) : 'a @ noalloc_strict = fss
[%%expect{|
val proj_ss : 'a noalloc_strict_field -> 'a @ noalloc_strict = <fun>
|}]

(* The [@@ noalloc] field is only capped at [noalloc], so projecting it as
   [noalloc_strict] fails. *)
let proj_n_too_strict ({ fn } : 'a noalloc_field @ alloc) : 'a @ noalloc_strict = fn
[%%expect{|
Line 1, characters 82-84:
1 | let proj_n_too_strict ({ fn } : 'a noalloc_field @ alloc) : 'a @ noalloc_strict = fn
                                                                                      ^^
Error: This value is "noalloc"
         because it is the field "fn" (with some modality) of the record at line 1, characters 23-29.
       However, the highlighted expression is expected to be "noalloc_strict".
|}]

let proj_n_too_strict ({ fn } : 'a noalloc_field @ noalloc) : 'a @ noalloc_strict = fn
[%%expect{|
Line 1, characters 84-86:
1 | let proj_n_too_strict ({ fn } : 'a noalloc_field @ noalloc) : 'a @ noalloc_strict = fn
                                                                                        ^^
Error: This value is "noalloc"
         because it is the field "fn" of the record at line 1, characters 23-29
         which is "noalloc".
       However, the highlighted expression is expected to be "noalloc_strict".
|}]

(* Construction requires the supplied value to satisfy the modality, so a
   [noalloc] value can be stored in a [@@ noalloc] field. *)
let mk_n (fn : 'a @ noalloc_strict) : 'a noalloc_field = { fn }
[%%expect{|
val mk_n : 'a @ noalloc_strict -> 'a noalloc_field = <fun>
|}]

let mk_n (fn : 'a @ noalloc) : 'a noalloc_field = { fn }
[%%expect{|
val mk_n : 'a @ noalloc -> 'a noalloc_field = <fun>
|}]

let mk_n (fss : 'a @ noalloc_strict) : 'a noalloc_strict_field = { fss }
[%%expect{|
val mk_n : 'a @ noalloc_strict -> 'a noalloc_strict_field = <fun>
|}]

(* CR shsong: the expect test is incorrect *)
(* ... but an [alloc] (default) value cannot. *)
let mk_n_fail (fn : 'a @ alloc) : 'a noalloc_field = { fn }
[%%expect{|
Line 1, characters 55-57:
1 | let mk_n_fail (fn : 'a @ alloc) : 'a noalloc_field = { fn }
                                                           ^^
Error: This value is "alloc"
       but is expected to be "noalloc"
         because it is the field "fn" (with some modality) of the record at line 1, characters 53-59.
|}]

let mk_n_fail (fss : 'a @ noalloc) : 'a noalloc_strict_field = { fss }
[%%expect{|
Line 1, characters 65-68:
1 | let mk_n_fail (fss : 'a @ noalloc) : 'a noalloc_strict_field = { fss }
                                                                     ^^^
Error: This value is "noalloc"
       but is expected to be "noalloc_strict"
         because it is the field "fss" (with some modality) of the record at line 1, characters 63-70.
|}]

let mk_n_fail (fss : 'a @ alloc) : 'a noalloc_strict_field = { fss }
[%%expect{|
Line 1, characters 63-66:
1 | let mk_n_fail (fss : 'a @ alloc) : 'a noalloc_strict_field = { fss }
                                                                   ^^^
Error: This value is "alloc"
       but is expected to be "noalloc_strict"
         because it is the field "fss" (with some modality) of the record at line 1, characters 61-68.
|}]

(* Modalities also appear on [val]s in a signature. The default ([alloc])
   prints without a modality, like [nonportable] for portability. *)
module type S = sig
  type t
  val foo : unit -> t @@ noalloc
  val bar : unit -> t @@ noalloc_strict
  val baz : unit -> t
end
[%%expect{|
module type S =
  sig
    type t
    val foo : unit -> t @@ noalloc
    val bar : unit -> t @@ noalloc_strict
    val baz : unit -> t
  end
|}]


(** Test 4: mode crossing for abstract types and concrete types *)

(* This mirrors the mode crossing tests for the other axes in
   [testsuite/tests/typing-modes/crossing.ml] (e.g. [cross_global] for
   locality, [cross_contended] for contention). The allocation axis is
   comonadic with order [noalloc_strict < noalloc < alloc] and legacy
   default [alloc]; declaring a smaller upper bound in the kind lets the
   type cross that part of the axis. *)

(* Abstract types: crossing is declared via the kind's [mod] bound.
   [mod alloc] is the top, so it adds no crossing and prints without an
   annotation, exactly like [cross_local] / [cross_nonportable]. *)
type cross_alloc : value mod alloc
type cross_noalloc : value mod noalloc
type cross_noalloc_strict : value mod noalloc_strict
[%%expect{|
type cross_alloc
type cross_noalloc : value mod noalloc
type cross_noalloc_strict : value mod noalloc_strict
|}]

(* [cross_noalloc_strict] crosses the whole axis: an [alloc] value can be
   used where [noalloc_strict] is expected. *)
let cross_noalloc_strict (x : cross_noalloc_strict @ alloc) : _ @ noalloc_strict = x
[%%expect{|
val cross_noalloc_strict :
  cross_noalloc_strict -> cross_noalloc_strict @ noalloc_strict = <fun>
|}]

let cross_noalloc_strict (x : cross_noalloc_strict @ alloc) : _ @ noalloc = x
[%%expect{|
val cross_noalloc_strict :
  cross_noalloc_strict -> cross_noalloc_strict @ noalloc = <fun>
|}]

(* [cross_noalloc] crosses only up to [noalloc]. *)
let cross_noalloc1 (x : cross_noalloc @ alloc) : _ @ noalloc = x
[%%expect{|
val cross_noalloc1 : cross_noalloc -> cross_noalloc @ noalloc = <fun>
|}]

(* ... but not all the way to [noalloc_strict]. *)
let cross_noalloc2 (x : cross_noalloc @ alloc) : _ @ noalloc_strict = x
[%%expect{|
Line 1, characters 70-71:
1 | let cross_noalloc2 (x : cross_noalloc @ alloc) : _ @ noalloc_strict = x
                                                                          ^
Error: This value is "noalloc" because it crosses with something
         which is "alloc".
       However, the highlighted expression is expected to be "noalloc_strict".
|}]

(* [cross_alloc] declares no crossing: it cannot be used at [noalloc]. *)
let cross_alloc (x : cross_alloc @ alloc) : _ @ noalloc = x
[%%expect{|
Line 1, characters 58-59:
1 | let cross_alloc (x : cross_alloc @ alloc) : _ @ noalloc = x
                                                              ^
Error: This value is "alloc" but is expected to be "noalloc".
|}]

let cross_alloc (x : cross_alloc @ alloc) : _ @ noalloc_strict = x
[%%expect{|
Line 1, characters 65-66:
1 | let cross_alloc (x : cross_alloc @ alloc) : _ @ noalloc_strict = x
                                                                     ^
Error: This value is "alloc" but is expected to be "noalloc_strict".
|}]

(* Concrete types: crossing is inferred from the type's structure rather
   than declared. *)

(* Plain immediate data crosses the whole allocation axis. *)
let cross_immediate (x : int @ alloc) : _ @ noalloc_strict = x
[%%expect{|
val cross_immediate : int -> int @ noalloc_strict = <fun>
|}]

let cross_immediate (x : int @ alloc) : _ @ noalloc = x
[%%expect{|
val cross_immediate : int -> int @ noalloc = <fun>
|}]

(* A concrete record of plain data also crosses the whole axis: it does
   not capture any allocation behaviour. *)
type record_t = { x : float; y : float }
let cross_record (x : record_t @ alloc) : _ @ noalloc_strict = x
[%%expect{|
type record_t = { x : float; y : float; }
val cross_record : record_t -> record_t @ noalloc_strict = <fun>
|}]

let cross_record (x : record_t @ alloc) : _ @ noalloc = x
[%%expect{|
val cross_record : record_t -> record_t @ noalloc = <fun>
|}]

(* Function types do not cross the allocation axis: an [alloc] function
   cannot be used where a [noalloc] one is expected. *)
let cross_fun (x : (unit -> unit) @ alloc) : _ @ noalloc = x
[%%expect{|
Line 1, characters 59-60:
1 | let cross_fun (x : (unit -> unit) @ alloc) : _ @ noalloc = x
                                                               ^
Error: This value is "alloc" but is expected to be "noalloc".
|}]

let cross_fun (x : (unit -> unit) @ alloc) : _ @ noalloc_strict = x
[%%expect{|
Line 1, characters 66-67:
1 | let cross_fun (x : (unit -> unit) @ alloc) : _ @ noalloc_strict = x
                                                                      ^
Error: This value is "alloc" but is expected to be "noalloc_strict".
|}]


(* Test 5: Other tests *)

type record_t = { x : float; y : float }
[%%expect{|
type record_t = { x : float; y : float; }
|}]

(* A function that returns a record. The [@ alloc] mode on the
   binding is the legacy default; the return mode is also default to [@ alloc] since it
   is ok to return the [@ noalloc] type as [@ alloc] *)
let (new_record @ alloc) () =
    ({ x = 3.0; y = 4.0 } : record_t @ noalloc)
[%%expect{|
val new_record : unit -> record_t = <fun>
|}]

(* The [@ noalloc] return mode needs to be explicitly specified *)
let f () : record_t @ noalloc = { x = 3.0; y = 4.0 }
[%%expect{|
val f : unit -> record_t @ noalloc = <fun>
|}]

(* CR-soon shsong: error expected -- f should not be accepted as [@ noalloc]
      Also need to figure out why M is [@@ noalloc_strict] *)
(* The allocation mode shows up in a [val] signature's function type. *)
module M : sig val f : unit -> record_t @ noalloc end = struct
    let (f @ noalloc_strict) () = { x = 3.0; y = 4.0 }
end
[%%expect{|
module M : sig val f : unit -> record_t @ noalloc end @@ stateless
  noalloc_strict
|}]

(* The allocation mode of a function-typed argument. *)
let apply (g : (unit -> record_t) @ noalloc) = g
[%%expect{|
val apply : (unit -> record_t) @ noalloc -> unit -> record_t = <fun>
|}]

(* Error triggered by mode capture*)
let noalloc_use : 'a @ noalloc -> unit = fun _ -> ()
let (allocates @ alloc) () = { x = 1.; y = 2. }
[%%expect{|
val noalloc_use : 'a @ noalloc -> unit = <fun>
val allocates : unit -> record_t = <fun>
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
val secretly_allocates : unit -> record_t = <fun>
|}]

(* CR-soon shsong: error expected -- cannot assign alloc func to escape_hatch ref *)
let (secretly_allocates' @ noalloc) () = exclave_
 let mutable escape_hatch = None in
 (escape_hatch <- stack_ Some (fun () -> { x = 1.; y = 2. })) [@zero_alloc];
 match escape_hatch with
 | None -> stack_ { x = 1.; y = 2. }
 | Some f -> f ()
[%%expect{|
val secretly_allocates' : unit -> record_t @ local = <fun>
|}]

(* CR-soon shsong: error expected -- cannot call [f ()] because it is alloc *)
let (secretly_allocates @ noalloc) () = exclave_
 let mutable (escape_hatch @ alloc) = None in
 (escape_hatch <- stack_ Some (fun () -> { x = 1.; y = 2. })) [@zero_alloc];
 match escape_hatch with
 | None -> stack_ { x = 1.; y = 2. }
 | Some f -> f ()
[%%expect{|
val secretly_allocates : unit -> record_t @ local = <fun>
|}]
