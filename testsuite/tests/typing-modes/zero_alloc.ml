(* TEST
 flags+="-extension mode_alpha -extension comprehensions";
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

(* The allocation axis is comonadic with order
   [noalloc_strict < noalloc < alloc] and legacy default [alloc];
   declaring a smaller upper bound in the kind lets the type cross
   that part of the axis. *)

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

(* Does an object type cross the Allocation axis? I.e. can an [alloc] object be
   used where [noalloc] / [noalloc_strict] is expected? (cf. Test 4: functions do
   NOT cross, immediates and records of immediates do.) *)
let cross_object (x : < get : int > @ alloc) : _ @ noalloc = x
[%%expect{|
Line 1, characters 61-62:
1 | let cross_object (x : < get : int > @ alloc) : _ @ noalloc = x
                                                                 ^
Error: This value is "alloc" but is expected to be "noalloc".
|}]
let cross_object_ss (x : < get : int > @ alloc) : _ @ noalloc_strict = x
[%%expect{|
Line 1, characters 71-72:
1 | let cross_object_ss (x : < get : int > @ alloc) : _ @ noalloc_strict = x
                                                                           ^
Error: This value is "alloc" but is expected to be "noalloc_strict".
|}]


(** Test 5.1: Allocation mode identifies all allocations and forces the proper
    mode. For each kind of allocation the typechecker can see, we test a
    [noalloc_strict] and a [noalloc] enclosing function side by side, and -- for
    the allocations that [stack_] supports -- a [stack_]-marked pair as well. An
    allocation currently forces the enclosing function to [alloc], which is
    above both [noalloc] and [noalloc_strict], so the detected cases error for
    both. For allocations that are moved to the stack by [stack_], no error
    is expected. *)

type record_t5 = { x : float; y : float }
type 'a variant_t5 = Nothing | Just of 'a
[%%expect{|
type record_t5 = { x : float; y : float; }
type 'a variant_t5 = Nothing | Just of 'a
|}]

(* Record construction allocates. *)
let (alloc_record @ noalloc_strict) () = { x = 1.; y = 2. }
[%%expect{|
Line 1, characters 41-59:
1 | let (alloc_record @ noalloc_strict) () = { x = 1.; y = 2. }
                                             ^^^^^^^^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 36-59
         which is expected to be "noalloc_strict".
|}]
let (alloc_record @ noalloc) () = { x = 1.; y = 2. }
[%%expect{|
Line 1, characters 34-52:
1 | let (alloc_record @ noalloc) () = { x = 1.; y = 2. }
                                      ^^^^^^^^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at line 1, characters 29-52
         which is expected to be "noalloc".
|}]

(* The same record allocation, but [stack_]-marked. *)
let (alloc_record @ noalloc_strict) () = exclave_ stack_ { x = 1.; y = 2. }
[%%expect{|
val alloc_record : unit -> record_t5 @ local = <fun>
|}]
let (alloc_record @ noalloc) () = exclave_ stack_ { x = 1.; y = 2. }
[%%expect{|
val alloc_record : unit -> record_t5 @ local = <fun>
|}]

(* A variant constructor with arguments allocates (constant constructors do
   not). *)
let (alloc_variant @ noalloc_strict) (a : int) = Just a
[%%expect{|
Line 1, characters 49-55:
1 | let (alloc_variant @ noalloc_strict) (a : int) = Just a
                                                     ^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 37-55
         which is expected to be "noalloc_strict".
|}]
let (alloc_variant @ noalloc) (a : int) = Just a
[%%expect{|
Line 1, characters 42-48:
1 | let (alloc_variant @ noalloc) (a : int) = Just a
                                              ^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at line 1, characters 30-48
         which is expected to be "noalloc".
|}]

(* The same variant allocation, but [stack_]-marked. *)
let (alloc_variant @ noalloc_strict) (a : int) = exclave_ stack_ (Just a)
[%%expect{|
val alloc_variant : int -> int variant_t5 @ local = <fun>
|}]
let (alloc_variant @ noalloc) (a : int) = exclave_ stack_ (Just a)
[%%expect{|
val alloc_variant : int -> int variant_t5 @ local = <fun>
|}]

(* A cons cell of a list allocates. *)
let (alloc_list @ noalloc_strict) (a : int) = [a]
[%%expect{|
Line 1, characters 46-49:
1 | let (alloc_list @ noalloc_strict) (a : int) = [a]
                                                  ^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 34-49
         which is expected to be "noalloc_strict".
|}]
let (alloc_list @ noalloc) (a : int) = [a]
[%%expect{|
Line 1, characters 39-42:
1 | let (alloc_list @ noalloc) (a : int) = [a]
                                           ^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at line 1, characters 27-42
         which is expected to be "noalloc".
|}]

(* The same list cell, but [stack_]-marked. *)
let (alloc_list @ noalloc_strict) (a : int) = exclave_ stack_ [a]
[%%expect{|
val alloc_list : int -> int list @ local = <fun>
|}]
let (alloc_list @ noalloc) (a : int) = exclave_ stack_ [a]
[%%expect{|
val alloc_list : int -> int list @ local = <fun>
|}]

(* An array literal allocates. *)
let (alloc_array @ noalloc_strict) (a : int) = [| a |]
[%%expect{|
Line 1, characters 47-54:
1 | let (alloc_array @ noalloc_strict) (a : int) = [| a |]
                                                   ^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 35-54
         which is expected to be "noalloc_strict".
|}]
let (alloc_array @ noalloc) (a : int) = [| a |]
[%%expect{|
Line 1, characters 40-47:
1 | let (alloc_array @ noalloc) (a : int) = [| a |]
                                            ^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at line 1, characters 28-47
         which is expected to be "noalloc".
|}]

(* The same array, but [stack_]-marked. *)
let (alloc_array @ noalloc_strict) (a : int) = exclave_ stack_ [| a |]
[%%expect{|
val alloc_array : int -> int array @ local = <fun>
|}]
let (alloc_array @ noalloc) (a : int) = exclave_ stack_ [| a |]
[%%expect{|
val alloc_array : int -> int array @ local = <fun>
|}]


(* A polymorphic variant with no argument does not allocate. *)
let (noalloc_polyvariant @ noalloc_strict) () = `Tag
[%%expect{|
val noalloc_polyvariant : unit -> [> `Tag ] = <fun>
|}]

(* A polymorphic variant with an argument allocates. *)
let (alloc_polyvariant @ noalloc_strict) (a : int) = `Tag a
[%%expect{|
Line 1, characters 53-59:
1 | let (alloc_polyvariant @ noalloc_strict) (a : int) = `Tag a
                                                         ^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 41-59
         which is expected to be "noalloc_strict".
|}]
let (alloc_polyvariant @ noalloc) (a : int) = `Tag a
[%%expect{|
Line 1, characters 46-52:
1 | let (alloc_polyvariant @ noalloc) (a : int) = `Tag a
                                                  ^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at line 1, characters 34-52
         which is expected to be "noalloc".
|}]

(* The same polymorphic variant, but [stack_]-marked. *)
let (alloc_polyvariant @ noalloc_strict) (a : int) = exclave_ stack_ (`Tag a)
[%%expect{|
val alloc_polyvariant : int -> [> `Tag of int ] @ local = <fun>
|}]
let (alloc_polyvariant @ noalloc) (a : int) = exclave_ stack_ (`Tag a)
[%%expect{|
val alloc_polyvariant : int -> [> `Tag of int ] @ local = <fun>
|}]

(* CR shsong: Currently it may also because multi-argument function
    always allocates. Revisit this in the next PR. *)
(* A function that takes an optional argument allocates (to handle the
   optional-argument wrapping). *)
let (alloc_optional_arg @ noalloc_strict) ?a () = a
[%%expect{|
val alloc_optional_arg : ?a:'a -> (unit -> 'a option) @ local = <fun>
|}]
let (alloc_optional_arg @ noalloc) ?a () = a
[%%expect{|
val alloc_optional_arg : ?a:'a -> (unit -> 'a option) @ local = <fun>
|}]

(* Building a closure that captures a variable allocates. *)
let (alloc_closure @ noalloc_strict) (a : int) = fun () -> a
[%%expect{|
Line 1, characters 49-60:
1 | let (alloc_closure @ noalloc_strict) (a : int) = fun () -> a
                                                     ^^^^^^^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]
let (alloc_closure @ noalloc) (a : int) = fun () -> a
[%%expect{|
Line 1, characters 42-53:
1 | let (alloc_closure @ noalloc) (a : int) = fun () -> a
                                              ^^^^^^^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

(* The same closure, but [stack_]-marked. *)
let (alloc_closure @ noalloc_strict) (a : int) = exclave_ stack_ (fun () -> a)
[%%expect{|
val alloc_closure : int -> (unit -> int) @ local = <fun>
|}]
let (alloc_closure @ noalloc) (a : int) = exclave_ stack_ (fun () -> a)
[%%expect{|
val alloc_closure : int -> (unit -> int) @ local = <fun>
|}]

(* Boxing a float read out of a flat float record allocates. *)
let (alloc_float_boxing @ noalloc_strict) (r : record_t5) = r.x
[%%expect{|
Line 1, characters 60-63:
1 | let (alloc_float_boxing @ noalloc_strict) (r : record_t5) = r.x
                                                                ^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 42-63
         which is expected to be "noalloc_strict".
|}]
let (alloc_float_boxing @ noalloc) (r : record_t5) = r.x
[%%expect{|
Line 1, characters 53-56:
1 | let (alloc_float_boxing @ noalloc) (r : record_t5) = r.x
                                                         ^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at line 1, characters 35-56
         which is expected to be "noalloc".
|}]

(* The same float boxing, but [stack_]-marked. *)
let (alloc_float_boxing @ noalloc_strict) (r : record_t5) = exclave_ stack_ r.x
[%%expect{|
val alloc_float_boxing : record_t5 -> float @ local = <fun>
|}]
let (alloc_float_boxing @ noalloc) (r : record_t5) = exclave_ stack_ r.x
[%%expect{|
val alloc_float_boxing : record_t5 -> float @ local = <fun>
|}]

(* Tuple construction allocates a boxed block. (We use a unit argument and constant
   elements to avoid an inner currying closure, which would otherwise be the
   real cause of an error.) *)
let (alloc_tuple @ noalloc_strict) () = (1, 2)
[%%expect{|
Line 1, characters 40-46:
1 | let (alloc_tuple @ noalloc_strict) () = (1, 2)
                                            ^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 35-46
         which is expected to be "noalloc_strict".
|}]
let (alloc_tuple @ noalloc) () = (1, 2)
[%%expect{|
Line 1, characters 33-39:
1 | let (alloc_tuple @ noalloc) () = (1, 2)
                                     ^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at line 1, characters 28-39
         which is expected to be "noalloc".
|}]

(* The same tuple, but [stack_]-marked. *)
let (alloc_tuple @ noalloc_strict) () = exclave_ stack_ (1, 2)
[%%expect{|
val alloc_tuple : unit -> int * int @ local = <fun>
|}]
let (alloc_tuple @ noalloc) () = exclave_ stack_ (1, 2)
[%%expect{|
val alloc_tuple : unit -> int * int @ local = <fun>
|}]

(* Partial application allocates a closure.
   We receive [f] as a parameter (rather than defining a multi-argument
   function, whose curried closures would themselves be flagged)
   so the only allocation here is the partial-application closure. *)
let (alloc_partial_app @ noalloc_strict)
      (f : (int -> int -> int -> int) @ noalloc_strict) = f 1 2
[%%expect{|
val alloc_partial_app :
  (int -> int -> int -> int) @ noalloc_strict -> int -> int = <fun>
|}]
let (alloc_partial_app @ noalloc)
      (f : (int -> int -> int -> int) @ noalloc) = f 1 2
[%%expect{|
val alloc_partial_app : (int -> int -> int -> int) @ noalloc -> int -> int =
  <fun>
|}]

(* [stack_] on a partial application is rejected: it is an application, not a
   recognized allocation form. [f] is received as a parameter to avoid the
   currying closures of a multi-argument definition. *)
let (stack_partial_app @ noalloc_strict)
      (f : (int -> int -> int -> int) @ noalloc_strict) = exclave_ stack_ (f 1 2)
[%%expect{|
Line 2, characters 74-81:
2 |       (f : (int -> int -> int -> int) @ noalloc_strict) = exclave_ stack_ (f 1 2)
                                                                              ^^^^^^^
Error: This expression is not an allocation site.
|}]

(* Partial application whose result is a function hidden behind a
   type abbreviation ([myfun = int -> int]). Here [f 1 2] is a partial
   application returning [myfun], so it allocates a closure. The detection uses
   [get_desc (expand_head env ty_ret)]: [expand_head] unfolds [myfun] to
   [int -> int] and the [Tarrow] is seen, so the allocation IS currently
   caught. *)
type myfun = int -> int
let (alloc_partial_app_abbrev @ noalloc_strict)
      (f : (int -> int -> myfun) @ noalloc_strict) = f 1 2
[%%expect{|
type myfun = int -> int
val alloc_partial_app_abbrev :
  (int -> int -> myfun) @ noalloc_strict -> myfun = <fun>
|}]

(* A function [f] with mixed arguments: optional [a], mandatory [b], optional
   [c], mandatory [d], optional [e], mandatory [g]. It ends in a mandatory
   argument [g], so it can be fully applied. It is received as a parameter so
   referencing it (in function position) does not itself trigger the capture
   rule; the only allocations come from how it is called. *)

(* Case 1: call with all mandatory args ([b], [d], [g]); all optionals are
   omitted, so they default to [None] (constant constructors, no [Some]
   wrapping). The call is fully applied (not partial), so it allocates nothing
   and is accepted. *)
let (call_all_mandatory @ noalloc_strict)
      (f : (?a:int -> int -> ?c:int -> int -> ?e:int -> int -> int)) =
  f 1 2 3
[%%expect{|
val call_all_mandatory :
  (?a:int -> int -> ?c:int -> int -> ?e:int -> int -> int) -> int = <fun>
|}]

(* Case 2: call with one optional ([~a:1]) and one mandatory ([b]). The mandatory
   args [d] and [g] are still missing, so this is a partial application, which
   allocates a closure and is rejected. (The provided [~a:1] would additionally
   be wrapped in [Some], but the partial-application check fires first.) *)
let (call_one_opt_one_mand @ noalloc_strict)
      (f : (?a:int -> int -> ?c:int -> int -> ?e:int -> int -> int)) =
  f ~a:1 2
[%%expect{|
Line 3, characters 7-8:
3 |   f ~a:1 2
           ^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at lines 2-3, characters 6-10
         which is expected to be "noalloc_strict".
|}]

(* Case 3: call a function whose last agument is optional argument and
   all mandatory arguments are applied. Since the last argument is optional
   the result still allocates. *)
let (call_one_opt_one_mand @ noalloc_strict)
      (f : (?a:int -> int -> ?c:int -> int -> ?e:int -> int)) =
  f ~a:1 2
[%%expect{|
Line 3, characters 7-8:
3 |   f ~a:1 2
           ^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at lines 2-3, characters 6-10
         which is expected to be "noalloc_strict".
|}]


(* A lazy block allocates on the heap *)
let (alloc_lazy @ noalloc_strict) (a : int) = lazy a
[%%expect{|
Line 1, characters 46-52:
1 | let (alloc_lazy @ noalloc_strict) (a : int) = lazy a
                                                  ^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 34-52
         which is expected to be "noalloc_strict".
|}]
let (alloc_lazy @ noalloc) (a : int) = lazy a
[%%expect{|
Line 1, characters 39-45:
1 | let (alloc_lazy @ noalloc) (a : int) = lazy a
                                           ^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at line 1, characters 27-45
         which is expected to be "noalloc".
|}]

(* [stack_] does not support lazy expressions. *)
let (stack_lazy @ noalloc_strict) (a : int) = exclave_ stack_ (lazy a)
[%%expect{|
Line 1, characters 62-70:
1 | let (stack_lazy @ noalloc_strict) (a : int) = exclave_ stack_ (lazy a)
                                                                  ^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 34-70
         which is expected to be "noalloc_strict".
|}]


(* An object allocates. *)
let (alloc_object @ noalloc_strict) () = object method m = 1 end
[%%expect{|
Line 1, characters 41-64:
1 | let (alloc_object @ noalloc_strict) () = object method m = 1 end
                                             ^^^^^^^^^^^^^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 36-64
         which is expected to be "noalloc_strict".
|}]
let (alloc_object @ noalloc) () = object method m = 1 end
[%%expect{|
Line 1, characters 34-57:
1 | let (alloc_object @ noalloc) () = object method m = 1 end
                                      ^^^^^^^^^^^^^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at line 1, characters 29-57
         which is expected to be "noalloc".
|}]

(* [stack_] does not support objects; here the object's method closure is
   itself a detected allocation, so the allocation-axis error fires on the
   method body before the unsupported-[stack_] check is reached. *)
let (stack_object @ noalloc_strict) () = exclave_ stack_ (object method m = 1 end)
[%%expect{|
Line 1, characters 57-82:
1 | let (stack_object @ noalloc_strict) () = exclave_ stack_ (object method m = 1 end)
                                                             ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 36-82
         which is expected to be "noalloc_strict".
|}]

(* An object with no allocating method (here only a value field) still
   allocates the object block itself. *)
let (obj_no_alloc_method @ noalloc_strict) () = object val x = 0 end
[%%expect{|
Line 1, characters 48-68:
1 | let (obj_no_alloc_method @ noalloc_strict) () = object val x = 0 end
                                                    ^^^^^^^^^^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 43-68
         which is expected to be "noalloc_strict".
|}]


(* Packing a first-class module [(module M)] allocates *)
module type S = sig end
module Empty : S = struct end
[%%expect{|
module type S = sig end
module Empty : S @@ stateless noalloc_strict
|}]
let (alloc_first_class_module @ noalloc_strict) () = (module Empty : S)
[%%expect{|
Line 1, characters 61-66:
1 | let (alloc_first_class_module @ noalloc_strict) () = (module Empty : S)
                                                                 ^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 48-71
         which is expected to be "noalloc_strict".
|}]
let (alloc_first_class_module @ noalloc) () = (module Empty : S)
[%%expect{|
Line 1, characters 54-59:
1 | let (alloc_first_class_module @ noalloc) () = (module Empty : S)
                                                          ^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at line 1, characters 41-64
         which is expected to be "noalloc".
|}]

(* [stack_] does not support first-class modules. *)
let (stack_first_class_module @ noalloc_strict) () = exclave_ stack_ (module Empty : S)
[%%expect{|
Line 1, characters 77-82:
1 | let (stack_first_class_module @ noalloc_strict) () = exclave_ stack_ (module Empty : S)
                                                                                 ^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 48-87
         which is expected to be "noalloc_strict".
|}]


(* An external/primitive is [alloc] by default, so referencing one is rejected:
   primitives are treated as allocating conservatively. *)
external my_extern : int -> int = "some_c_stub"
[%%expect{|
external my_extern : int -> int = "some_c_stub"
|}]
let (alloc_external @ noalloc_strict) (a : int) = my_extern a
[%%expect{|
Line 1, characters 50-59:
1 | let (alloc_external @ noalloc_strict) (a : int) = my_extern a
                                                      ^^^^^^^^^
Error: The value "my_extern" is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 38-61
         which is expected to be "noalloc_strict".
|}]
let (alloc_external @ noalloc) (a : int) = my_extern a
[%%expect{|
Line 1, characters 43-52:
1 | let (alloc_external @ noalloc) (a : int) = my_extern a
                                               ^^^^^^^^^
Error: The value "my_extern" is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at line 1, characters 31-54
         which is expected to be "noalloc".
|}]

(* Arithmetic boxing is handled conservatively: the arithmetic operators are
   themselves [alloc] values, so using one is rejected even though the box is
   created in the backend. No allocation is missed. *)
let (alloc_float_arith @ noalloc_strict) (a : float) = a +. a
[%%expect{|
Line 1, characters 57-59:
1 | let (alloc_float_arith @ noalloc_strict) (a : float) = a +. a
                                                             ^^
Error: The value "(+.)" is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 41-61
         which is expected to be "noalloc_strict".
|}]
let (alloc_float_arith @ noalloc) (a : float) = a +. a
[%%expect{|
Line 1, characters 50-52:
1 | let (alloc_float_arith @ noalloc) (a : float) = a +. a
                                                      ^^
Error: The value "(+.)" is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at line 1, characters 34-54
         which is expected to be "noalloc".
|}]

(* Likewise for boxed-integer arithmetic via a [Stdlib] primitive. *)
let (alloc_int64_arith @ noalloc_strict) (a : int64) = Int64.add a a
[%%expect{|
Line 1, characters 55-64:
1 | let (alloc_int64_arith @ noalloc_strict) (a : int64) = Int64.add a a
                                                           ^^^^^^^^^
Error: The value "Int64.add" is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 41-68
         which is expected to be "noalloc_strict".
|}]
let (alloc_int64_arith @ noalloc) (a : int64) = Int64.add a a
[%%expect{|
Line 1, characters 48-57:
1 | let (alloc_int64_arith @ noalloc) (a : int64) = Int64.add a a
                                                    ^^^^^^^^^
Error: The value "Int64.add" is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at line 1, characters 34-61
         which is expected to be "noalloc".
|}]

(* CR-soon shsong: revisit exception handling after implementing the
    part that distinguishes noalloc_strict and noalloc *)
(* Constructing an exception (an extensible variant) with an argument
   allocates. *)
let (alloc_exception @ noalloc_strict) (a : string) = Failure a
[%%expect{|
Line 1, characters 54-63:
1 | let (alloc_exception @ noalloc_strict) (a : string) = Failure a
                                                          ^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 39-63
         which is expected to be "noalloc_strict".
|}]
let (alloc_exception @ noalloc) (a : string) = Failure a
[%%expect{|
Line 1, characters 47-56:
1 | let (alloc_exception @ noalloc) (a : string) = Failure a
                                                   ^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at line 1, characters 32-56
         which is expected to be "noalloc".
|}]

(* The same exception construction, but [stack_]-marked. *)
let (alloc_exception @ noalloc_strict) (a : string) = exclave_ stack_ (Failure a)
[%%expect{|
val alloc_exception : string -> exn @ local = <fun>
|}]
let (alloc_exception @ noalloc) (a : string) = exclave_ stack_ (Failure a)
[%%expect{|
val alloc_exception : string -> exn @ local = <fun>
|}]

(* Reading an atomic record field via [%atomic.loc] allocates an atomic
   location. *)
type atomic_record = { mutable af : string [@atomic] }
[%%expect{|
type atomic_record = { mutable af : string [@atomic]; }
|}]
let (alloc_atomic_loc @ noalloc_strict) (r : atomic_record) =
  [%atomic.loc r.af]
[%%expect{|
Line 2, characters 2-20:
2 |   [%atomic.loc r.af]
      ^^^^^^^^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at lines 1-2, characters 40-20
         which is expected to be "noalloc_strict".
|}]
let (alloc_atomic_loc @ noalloc) (r : atomic_record) =
  [%atomic.loc r.af]
[%%expect{|
Line 2, characters 2-20:
2 |   [%atomic.loc r.af]
      ^^^^^^^^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at lines 1-2, characters 33-20
         which is expected to be "noalloc".
|}]

(* CR-soon shsong: check currying-related behavior later *)
(* Coercing a function with an optional argument to a plain arrow eta-expands
   it, allocating a closure. We receive both functions as parameters (in a
   tuple to avoid currying closures) so the only allocation is the coercion
   closure. *)
let (alloc_fun_coerce @ noalloc_strict)
      ((apply, opt) :
         (((unit -> int) -> int) * (?x:int -> unit -> int)) @ noalloc_strict) =
  apply opt
[%%expect{|
Line 4, characters 8-11:
4 |   apply opt
            ^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at lines 2-4, characters 6-11
         which is expected to be "noalloc_strict".
|}]
let (alloc_fun_coerce @ noalloc)
      ((apply, opt) :
         (((unit -> int) -> int) * (?x:int -> unit -> int)) @ noalloc) =
  apply opt
[%%expect{|
Line 4, characters 8-11:
4 |   apply opt
            ^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at lines 2-4, characters 6-11
         which is expected to be "noalloc".
|}]

(* A list/array comprehension allocates its result *)
let (alloc_list_comprehension @ noalloc_strict) () = [ x for x = 1 to 10 ]
[%%expect{|
Line 1, characters 53-74:
1 | let (alloc_list_comprehension @ noalloc_strict) () = [ x for x = 1 to 10 ]
                                                         ^^^^^^^^^^^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 48-74
         which is expected to be "noalloc_strict".
|}]
let (alloc_list_comprehension @ noalloc) () = [ x for x = 1 to 10 ]
[%%expect{|
Line 1, characters 46-67:
1 | let (alloc_list_comprehension @ noalloc) () = [ x for x = 1 to 10 ]
                                                  ^^^^^^^^^^^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at line 1, characters 41-67
         which is expected to be "noalloc".
|}]

(* [stack_] does not support comprehensions. *)
let (stack_comprehension @ noalloc_strict) () = exclave_ stack_ [ x for x = 1 to 10 ]
[%%expect{|
Line 1, characters 64-85:
1 | let (stack_comprehension @ noalloc_strict) () = exclave_ stack_ [ x for x = 1 to 10 ]
                                                                    ^^^^^^^^^^^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 43-85
         which is expected to be "noalloc_strict".
|}]

(* Only the first case below is a genuine bug introduced by routing the
   lock-walk through [register_allocation_mode]. The others turned out NOT to be
   problems and are kept for contrast. *)

(* CR shsong (false positive): array indexing uses the [%array_safe_get]
   primitive, whose result is [@local_opt] (Prim_poly). It does NOT allocate, but
   the Prim_poly registration in [type_ident] goes through
   [register_allocation_mode], which now walks locks, so it is wrongly rejected
   -- note the error is "The allocation is alloc". It should be accepted once the
   Prim_poly site is exempted from walking. *)
let array_get_prim_poly (a : int array) (i : int) =
  let (f @ noalloc_strict) () = a.(i) in
  f ()
[%%expect{|
Line 2, characters 32-37:
2 |   let (f @ noalloc_strict) () = a.(i) in
                                    ^^^^^
Error: The value "Array.get" is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 2, characters 27-37
         which is expected to be "noalloc_strict".
|}]

(* NOT the Prim_poly bug: referencing a primitive/operator as a value is rejected
   by the pre-existing capture rule (primitives are [alloc] by default) -- note
   "The value ... is alloc", not "The allocation is alloc". Same conservative
   behavior as the external/arithmetic tests above; independent of Prim_poly.
   ([my_id]/[!] do not allocate, but referencing the value is conservatively
   treated as [alloc].) *)
external my_id : ('a[@local_opt]) -> ('a[@local_opt]) = "%identity"
let (prim_value_captured @ noalloc_strict) (x : int) = my_id x
[%%expect{|
external my_id : ('a [@local_opt]) -> ('a [@local_opt]) = "%identity"
Line 2, characters 55-60:
2 | let (prim_value_captured @ noalloc_strict) (x : int) = my_id x
                                                           ^^^^^
Error: The value "my_id" is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 2, characters 43-62
         which is expected to be "noalloc_strict".
|}]
let (deref_value_captured @ noalloc_strict) (r : int ref) = !r
[%%expect{|
Line 1, characters 60-61:
1 | let (deref_value_captured @ noalloc_strict) (r : int ref) = !r
                                                                ^
Error: The value "(!)" is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 44-62
         which is expected to be "noalloc_strict".
|}]

(* NOT a gap: [new] does not register the object allocation, but a class is
   always at the legacy ([alloc]) mode and the lock walk propagates that, so the
   instantiation is still (conservatively) rejected. *)
class cls = object method m = 1 end
let (alloc_new @ noalloc_strict) () = new cls
[%%expect{|
class cls : object method m : int end
Line 2, characters 42-45:
2 | let (alloc_new @ noalloc_strict) () = new cls
                                              ^^^
Error: The class "cls" is "alloc" because classes are always at the legacy modes.
       However, the class "cls" highlighted is expected to be "noalloc_strict"
         because it is used inside the function at line 2, characters 33-45
         which is expected to be "noalloc_strict".
|}]

(* A format string literal used at [format] type is built into a
   [CamlinternalFormatBasics] value, which allocates. This IS detected (the
   format is built as a constructor application that registers an allocation),
   so the enclosing function is correctly rejected. *)
let (use_format @ noalloc_strict) () : (_, _, _) format = "%d"
[%%expect{|
Line 1, characters 58-62:
1 | let (use_format @ noalloc_strict) () : (_, _, _) format = "%d"
                                                              ^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 34-62
         which is expected to be "noalloc_strict".
|}]


(** Test 5.2: an allocation nested inside several enclosing functions forces
    every enclosing layer. The functions [g] and [h] each capture the parameter
    [a], so building each inner closure is itself an allocation in its parent;
    together with the innermost [Just a] this makes every one of the three nested
    functions an allocation site, so each is forced to [alloc]. *)

(* All three layers annotated [noalloc_strict]: the capture chain reaches the
   outermost function, so building the closure [g] (which transitively captures
   [a]) is an allocation inside the outermost function and is reported there --
   demonstrating that the deepest allocation forces every enclosing layer, not
   just the immediately enclosing one. *)
let (layers3_ss @ noalloc_strict) (a : int) =
  let (g @ noalloc_strict) () =
    let (h @ noalloc_strict) () = Just a in
    h
  in
  g
[%%expect{|
Lines 13-15, characters 27-5:
13 | ...........................() =
14 |     let (h @ noalloc_strict) () = Just a in
15 |     h
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at lines 12-17, characters 34-3
         which is expected to be "noalloc_strict".
|}]

(* The same nesting, all layers [noalloc]. *)
let (layers3_n @ noalloc) (a : int) =
  let (g @ noalloc) () =
    let (h @ noalloc) () = Just a in
    h
  in
  g
[%%expect{|
Lines 2-4, characters 20-5:
2 | ....................() =
3 |     let (h @ noalloc) () = Just a in
4 |     h
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at lines 1-6, characters 26-3
         which is expected to be "noalloc".
|}]

(* Annotating only the middle layer still errors: building the closure [h]
   inside [g] is an allocation that forces [g]. *)
let layers_middle (a : int) =
  let (g @ noalloc_strict) () =
    let h () = Just a in
    h
  in
  g
[%%expect{|
Line 3, characters 10-21:
3 |     let h () = Just a in
              ^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at lines 2-4, characters 27-5
         which is expected to be "noalloc_strict".
|}]

(* Annotating only the innermost layer still errors: [Just a] forces [h]. *)
let layers_inner (a : int) =
  let g () =
    let (h @ noalloc_strict) () = Just a in
    h
  in
  g
[%%expect{|
Line 3, characters 34-40:
3 |     let (h @ noalloc_strict) () = Just a in
                                      ^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 3, characters 29-40
         which is expected to be "noalloc_strict".
|}]

(** Test 5.3: [stack_] negative cases *)

(* [stack_] on a non-allocation (a plain value) is rejected: it is not an
   allocation. *)
let (stack_non_alloc @ noalloc_strict) (a : int) = exclave_ stack_ a
[%%expect{|
Line 5, characters 67-68:
5 | let (stack_non_alloc @ noalloc_strict) (a : int) = exclave_ stack_ a
                                                                       ^
Error: This expression is not an allocation site.
|}]

(** Test 5.4: [stack_] nested cases. *)
(* Nested: an inner allocation that is not itself [stack_]-marked still
   allocates on the heap, even when an enclosing allocation is [stack_]-marked. *)
let (stack_nested @ noalloc_strict) (a : int) = exclave_ stack_ (Just (Just a))
[%%expect{|
Line 1, characters 70-78:
1 | let (stack_nested @ noalloc_strict) (a : int) = exclave_ stack_ (Just (Just a))
                                                                          ^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 36-79
         which is expected to be "noalloc_strict".
|}]

(* A [@@ global] field forces its value onto the heap even when the enclosing
   record is [stack_]-allocated: the record block is exempt, but the global
   field value [Just a] is a genuine heap allocation and is still rejected. This
   is another way (besides plain nesting above) for a heap allocation to sit
   inside a [stack_] one. *)
type 'a global_field_rec = { gf : 'a @@ global; rest : int }
let (stack_global_field @ noalloc_strict) (a : int) =
  exclave_ stack_ { gf = Just a; rest = a }
[%%expect{|
type 'a global_field_rec = { gf : 'a @@ global; rest : int; }
Line 3, characters 25-31:
3 |   exclave_ stack_ { gf = Just a; rest = a }
                             ^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at lines 2-3, characters 42-43
         which is expected to be "noalloc_strict".
|}]


(** Test 5.5: the allocation axis is exempted only by [stack_]
    ([expected_mode.strictly_stack]), not by [local_]/[exclave_]
    ([expected_mode.strictly_local]). The two flags are independent, so they are
    NOT interchangeable:

    - [strictly_stack] is set ONLY by the direct operand of [stack_], the
      explicit, checked stack-allocation form. It can be [true] while
      [strictly_local] is [false] -- e.g. a [stack_] in a plain [let] binding,
      not under any [exclave_]/[local_]. Gating the walk on [strictly_local]
      would WRONGLY REJECT such a legitimate [stack_] (see [stack_in_let]).
    - [strictly_local] is set by [local_]/[exclave_], which only constrain the
      *mode* to [local]. The current implementation does not treat them as
      exempt (it conservatively requires the explicit, checked [stack_] marker),
      so these still error (see [exclave_not_exempt]/[local_not_exempt]). *)

(* [exclave_] alone (no [stack_]) does not exempt the allocation. *)
let (exclave_not_exempt @ noalloc_strict) (a : int) = exclave_ (Just a)
[%%expect{|
Line 17, characters 63-71:
17 | let (exclave_not_exempt @ noalloc_strict) (a : int) = exclave_ (Just a)
                                                                    ^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 17, characters 42-71
         which is expected to be "noalloc_strict".
|}]

(* [local_] alone (no [stack_]) does not exempt the allocation. *)
let (local_not_exempt @ noalloc_strict) (a : int) =
  let _x = local_ (Just a) in
  ()
[%%expect{|
Line 2, characters 18-26:
2 |   let _x = local_ (Just a) in
                      ^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at lines 1-3, characters 40-4
         which is expected to be "noalloc_strict".
|}]

(* For contrast, the same allocation wrapped in [stack_] (sets [strictly_stack])
   is exempt and accepted. *)
let (stack_is_exempt @ noalloc_strict) (a : int) = exclave_ stack_ (Just a)
[%%expect{|
val stack_is_exempt : int -> int variant_t5 @ local = <fun>
|}]

(* The decisive case: a [stack_] in a plain [let] binding (no [exclave_]/[local_]
   around it) has [strictly_stack = true] but [strictly_local = false]. It is
   correctly accepted. Gating the walk on [strictly_local] would reject it, which
   is why [strictly_stack] is required rather than reusing [strictly_local]. *)
let (stack_in_let @ noalloc_strict) (a : int) =
  let _x = stack_ (Just a) in
  ()
[%%expect{|
val stack_in_let : int -> unit = <fun>
|}]


(** Test 5.6: a transparent wrapper around the [stack_] operand should not defeat
    the [stack_] exemption.

    [stack_] sets [strictly_stack] on its operand.
    Although we do not want [strictly_stack = true] for children allocations,
    we should not use [mode_morph]/[mode_coerce] to reset [strictly_stack] for
    children (while keeping [strictly_local]). *)

(* A type annotation goes through [Pexp_constraint] -> [type_expect_mode]
   -> [mode_coerce], which should not resets [strictly_stack] on the
   inner allocation, i.e., the inner allocation is still on stack. *)
let (stack_through_type_annot @ noalloc_strict) (a : int) =
  exclave_ stack_ ((Just a : int variant_t5))
[%%expect{|
val stack_through_type_annot : int -> int variant_t5 @ local = <fun>
|}]

(* Same via a mode annotation. *)
let (stack_through_mode_annot @ noalloc_strict) (a : int) =
  exclave_ stack_ ((Just a : _ @ local))
[%%expect{|
val stack_through_mode_annot : int -> int variant_t5 @ local = <fun>
|}]

(* A coercion [:>] does NOT reset [strictly_stack]: the [stack_]
   operand stays exempt and is correctly accepted. *)
let (stack_through_coercion @ noalloc_strict) (a : int) =
  exclave_ stack_ ((Just a :> int variant_t5))
[%%expect{|
val stack_through_coercion : int -> int variant_t5 @ local = <fun>
|}]

(* An attribute on the allocation is also fine. *)
let (stack_through_attribute @ noalloc_strict) (a : int) =
  exclave_ stack_ ((Just a)[@inline])
[%%expect{|
val stack_through_attribute : int -> int variant_t5 @ local = <fun>
|}]

(* [let open ... in] is also transparent, but here [stack_]
   fails to even recognize the allocation site -- the [Pexp_stack]
   allocation-site detection does not see through [Pexp_open] (this is a
   detection gap, not the [strictly_stack] reset issue). *)
let (stack_through_open @ noalloc_strict) (a : int) =
  exclave_ stack_ (let open Stdlib in Just a)
[%%expect{|
Line 2, characters 18-45:
2 |   exclave_ stack_ (let open Stdlib in Just a)
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression is not an allocation site.
|}]


(** Test 5.7: a genuinely NEW inner allocation under a [stack_] outer is still
    flagged. Unlike a transparent wrapper (Test 5.6), these inner expressions
    introduce their own allocation, so the [stack_] exemption must NOT reach
    them. It does not: [stack_] only marks its top operand, and every inner
    allocation below is registered with [strictly_stack = false] -- either
    because its mode is built by [mode_default] (constructor args, tuple
    components, record fields, function bodies) or because [mode_morph] resets
    [strictly_stack] for children (lazy thunk body, partial-application args). *)

(* a new allocation captured as an argument of a function application *)
let stack_inner_function_application
      (g : (int variant_t5 -> int -> int -> int) @ noalloc_strict) (a : int) =
  let (f @ noalloc_strict) () = exclave_ stack_ (g (Just a) 2 3) in
  ()
[%%expect{|
Line 13, characters 51-59:
13 |   let (f @ noalloc_strict) () = exclave_ stack_ (g (Just a) 2 3) in
                                                        ^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 13, characters 27-64
         which is expected to be "noalloc_strict".
|}]

(* CR shsong: function partial application triggers allocation, and is not
    considered to be on the stack even though it is annotated with [stack_].
    Revisit this in the next PR. *)
(* a new allocation captured as an argument of a partial application *)
let stack_inner_partial_application
      (g : (int variant_t5 -> int -> int -> int) @ noalloc_strict) (a : int) =
  let (f @ noalloc_strict) () = exclave_ stack_ (g (Just a) 2) in
  ()
[%%expect{|
Line 3, characters 51-59:
3 |   let (f @ noalloc_strict) () = exclave_ stack_ (g (Just a) 2) in
                                                       ^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 3, characters 27-62
         which is expected to be "noalloc_strict".
|}]

(* a tuple component is a new allocation *)
let (stack_inner_tuple @ noalloc_strict) (a : int) =
  exclave_ stack_ (Just a, Just a)
[%%expect{|
Line 2, characters 19-25:
2 |   exclave_ stack_ (Just a, Just a)
                       ^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at lines 1-2, characters 41-34
         which is expected to be "noalloc_strict".
|}]

(* a record field is a new allocation *)
type stack_inner_rec = { sf1 : int variant_t5; sf2 : int }
let (stack_inner_field @ noalloc_strict) (a : int) =
  exclave_ stack_ { sf1 = Just a; sf2 = a }
[%%expect{|
type stack_inner_rec = { sf1 : int variant_t5; sf2 : int; }
Line 3, characters 26-32:
3 |   exclave_ stack_ { sf1 = Just a; sf2 = a }
                              ^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at lines 2-3, characters 41-43
         which is expected to be "noalloc_strict".
|}]

(* a function body has a new allocation *)
let (stack_inner_funbody @ noalloc_strict) (a : int) =
  exclave_ stack_ (fun () -> Just a)
[%%expect{|
Line 2, characters 29-35:
2 |   exclave_ stack_ (fun () -> Just a)
                                 ^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at lines 1-2, characters 43-36
         which is expected to be "noalloc_strict".
|}]


(** Test 5.8: regression guard for making [stack_] set [strictly_local].

    A [stack_] closure placed where a [global] value is required must report a
    clean locality error, NOT crash. If the [Pexp_stack] handler is changed to
    [mode_strictly_stack expected_mode |> mode_strictly_local], then
    [split_function_ty] forces the closure areality with [Locality.submode_exn]
    (line ~6237). That submode genuinely fails here (the closure must be global),
    and the [_exn] form raises [Invalid_argument "result is Error _"] -- a
    compiler crash -- instead of the graceful error below. With the handler left
    as [mode_strictly_stack] only, all four error cleanly. *)

(* stack_ closure stored in a ref (ref contents must be global) *)
let stack_clo_in_ref = ref (stack_ (fun x -> x))
[%%expect{|
Line 13, characters 27-48:
13 | let stack_clo_in_ref = ref (stack_ (fun x -> x))
                                ^^^^^^^^^^^^^^^^^^^^^
Error: This value is "local" because it is "stack_"-allocated.
       However, the highlighted expression is expected to be "global".
|}]

(* stack_ closure as the function in a tail call *)
let stack_clo_tailcall () = (stack_ (fun x -> x)) 42
[%%expect{|
Line 1, characters 28-49:
1 | let stack_clo_tailcall () = (stack_ (fun x -> x)) 42
                                ^^^^^^^^^^^^^^^^^^^^^
Error: This value is "local" because it is "stack_"-allocated.
       However, the highlighted expression is expected to be "local" to the parent region or "global"
         because it is the function in a tail call.
|}]

(* stack_ multi-argument closure stored in a ref *)
let stack_clo_multiarg () = ref (stack_ (fun x y -> x : 'a -> 'a -> 'a))
[%%expect{|
Line 1, characters 32-72:
1 | let stack_clo_multiarg () = ref (stack_ (fun x y -> x : 'a -> 'a -> 'a))
                                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "local" because it is "stack_"-allocated.
       However, the highlighted expression is expected to be "global".
|}]

(* stack_ closure returned where a global function is expected *)
let stack_clo_global_return () : int -> int = stack_ (fun x -> x)
[%%expect{|
Line 1, characters 46-65:
1 | let stack_clo_global_return () : int -> int = stack_ (fun x -> x)
                                                  ^^^^^^^^^^^^^^^^^^^
Error: This value is "local" because it is "stack_"-allocated.
       However, the highlighted expression is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]


(** Test 5.9: annotating the codomain of the arrow with [@ noalloc_strict]
    constrains the mode of the application result. With
    [foo : int -> t @ noalloc_strict], applying [M.foo 1] yields a
    [noalloc_strict] value (and likewise [M.foo_int 1]). Note the codomain mode
    [@ noalloc_strict] must precede the [@@ noalloc_strict] modality; writing it
    as [(t @ noalloc_strict) @@ ...] is a syntax error. *)

module M : sig
  type t
  val foo : int -> t @ noalloc_strict @@ noalloc_strict

  type int_t
  val foo_int : int -> int_t @ noalloc_strict @@ noalloc_strict
  val foo_int_default : int -> int_t @@ noalloc_strict
end = struct
  type t = (int -> unit) -> unit
  let g0 : t = fun g -> g 0
  let foo (_ : int) = g0

  type int_t = int
  let foo_int x = x
  let foo_int_default x = x
end
[%%expect{|
module M :
  sig
    type t
    val foo : int -> t @ noalloc_strict @@ noalloc_strict
    type int_t
    val foo_int : int -> int_t @ noalloc_strict @@ noalloc_strict
    val foo_int_default : int -> int_t @@ noalloc_strict
  end @@ stateless noalloc_strict
|}]

(* CR shsong: Error expected -- [M.foo 1] is a partial application,
    which causes allocation. The current partial application detection
    cannot detect whether an abstract type is an arrow type. *)
let apply_one () = (M.foo 1 : _ @ noalloc_strict)
[%%expect{|
val apply_one : unit -> M.t = <fun>
|}]

let apply_zero () = (M.foo : _ @ noalloc_strict)
[%%expect{|
val apply_zero : unit -> int -> M.t @ noalloc_strict = <fun>
|}]

let apply_foo_int () = (M.foo_int 1 : _ @ noalloc_strict)
[%%expect{|
val apply_foo_int : unit -> M.int_t = <fun>
|}]

let apply_foo_int_default () = (M.foo_int_default 1 : _ @ noalloc_strict)
[%%expect{|
Line 1, characters 32-51:
1 | let apply_foo_int_default () = (M.foo_int_default 1 : _ @ noalloc_strict)
                                    ^^^^^^^^^^^^^^^^^^^
Error: This value is "alloc" but is expected to be "noalloc_strict".
|}]


(** Test 6: Muti-argument function (currying) and partial application *)

(** Test 6.1: a curried multi-argument function can be [noalloc]/
  [noalloc_strict], but the inner closure (its codomain) must be [local]. *)

let (f @ noalloc_strict) x y = x
[%%expect{|
val f : 'a -> ('b -> 'a) @ local = <fun>
|}]

let (f_explicit @ noalloc_strict) x = fun y -> x
[%%expect{|
Line 1, characters 38-48:
1 | let (f_explicit @ noalloc_strict) x = fun y -> x
                                          ^^^^^^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

let (f_explicit_exclave @ noalloc_strict) x = exclave_ fun y -> x
[%%expect{|
val f_explicit_exclave : 'a -> ('b -> 'a) @ local = <fun>
|}]

let (f @ noalloc) x y = x
[%%expect{|
val f : 'a -> ('b -> 'a) @ local = <fun>
|}]

let (f_explicit @ noalloc) x = fun y -> x
[%%expect{|
Line 1, characters 31-41:
1 | let (f_explicit @ noalloc) x = fun y -> x
                                   ^^^^^^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

let (f_explicit_exclave @ noalloc) x = exclave_ fun y -> x
[%%expect{|
val f_explicit_exclave : 'a -> ('b -> 'a) @ local = <fun>
|}]

(* Contrast: a single-argument function builds no inner closure.
  Its return type is not a closure, so it can be global. *)
let (g @ noalloc_strict) x = x
[%%expect{|
val g : 'a -> 'a = <fun>
|}]

let (g @ noalloc) x = x
[%%expect{|
val g : 'a -> 'a = <fun>
|}]

(* Soundness: the returned closure is pinned [local], so it cannot escape to a
  [global] (heap) position -- using [f] where a [global] codomain is expected
  is rejected. *)
let (f @ noalloc_strict) x y = x
let escapes = (f : _ -> (_ -> _) @ global)
[%%expect{|
val f : 'a -> ('b -> 'a) @ local = <fun>
Line 2, characters 15-16:
2 | let escapes = (f : _ -> (_ -> _) @ global)
                   ^
Error: The value "f" has type "'a -> ('b -> 'a) @ local"
       but an expression was expected of type "'c -> 'd -> 'e"
|}]

(* The local requirement is passed down the whole curry chain:
  a three-argument function is accepted, with [local] mode at each layer of
  codomain. Although [local] for inner layers is not printed, we have the
  following sanity checks to validate this. *)
let (f3 @ noalloc_strict) x y z = x
[%%expect{|
val f3 : 'a -> ('b -> 'c -> 'a) @ local = <fun>
|}]

(* Sanity check: [c' -> 'a] above is local and cannot be returned *)
let g () = f3 0 1
[%%expect{|
Line 1, characters 11-17:
1 | let g () = f3 0 1
               ^^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
Hint: This is a partial application
      Adding 1 more argument will make the value non-local
|}]

(* Sanity check: Prove local constraints on inner steps with coercions.
  Requiring the inner arrow ([_ -> _], the result of [f3 x y]) to be
  [local] is accepted. *)
let f3_inner_local = (f3 : _ -> ((_ -> (_ -> _) @ local)) @ local)
[%%expect{|
val f3_inner_local : 'a -> ('b -> 'c -> 'a) @ local = <fun>
|}]

(* Sanity check: Conversely, requiring that same inner arrow to be [global]
  is rejected: it is really [local]. This is the definitive evidence that
  the propagation reaches the inner step; the missing [@ local] in the
  printed signature above is only a type-printer elision. *)
let f3_inner_not_global = (f3 : _ -> ((_ -> (_ -> _) @ global)) @ local)
[%%expect{|
Line 1, characters 27-29:
1 | let f3_inner_not_global = (f3 : _ -> ((_ -> (_ -> _) @ global)) @ local)
                               ^^
Error: The value "f3" has type "'a -> ('b -> 'c -> 'a) @ local"
       but an expression was expected of type
         "'a -> ('d -> ('e -> 'f)) @ local"
       Type "'b -> ('c -> 'a) @ local" is not compatible with type
         "'d -> 'e -> 'f"
|}]

(* Soundness: a genuine allocation in the body (here a tuple) is still counted;
   only the curried intermediate closures are exempt. *)
let (f_body_alloc @ noalloc_strict) x y = (x, y)
[%%expect{|
Line 1, characters 42-48:
1 | let (f_body_alloc @ noalloc_strict) x y = (x, y)
                                              ^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 36-48
         which is expected to be "noalloc_strict".
|}]

(** Test 6.2: forcing the codomain of a [noalloc] curried function to [local]
    interacts with an explicit codomain areality annotation. Because the
    codomain arrow is forced to [local], an explicit [@ global] on it conflicts,
    while an explicit [@ local] agrees (and the function is accepted). *)

(* The codomain is forced to [local], which conflicts with the explicit
   [@ global] on the returned arrow. *)
let (f : int -> (int -> int) @ global) @ noalloc_strict = fun x y -> x
[%%expect{|
Line 8, characters 64-70:
8 | let (f : int -> (int -> int) @ global) @ noalloc_strict = fun x y -> x
                                                                    ^^^^^^
Error: This value is "local" but is expected to be "global".
|}]

(* Contrast: without the [noalloc_strict] annotation the codomain is NOT forced,
   so the same [@ global] arrow is accepted. *)
let (f : int -> (int -> int) @ global) = fun x y -> x
[%%expect{|
val f : int -> int -> int = <fun>
|}]

(* Annotating the codomain [@ local] agrees with the forcing, so the function
   is accepted. *)
let (f : int -> (int -> int) @ local) @ noalloc_strict = fun x y -> x
[%%expect{|
val f : int -> (int -> int) @ local = <fun>
|}]

(* Sanity check: [f 0] is local *)
let g () = f 0
[%%expect{|
Line 1, characters 11-14:
1 | let g () = f 0
               ^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
Hint: This is a partial application
      Adding 1 more argument will make the value non-local
|}]

(** Test 6.3: propagation of [strictly_local_closure] to inner layers.

    A closure built inside a [noalloc_strict] / [noalloc] function (if
    the information is known when type check the closure) is forced to be
    [local] and on stack. It is also exempted from the allocation check.
    We probe each syntactic position for a nested closure.

    Reading the results:
    - Acceptance (a [val ... = <fun>] with a [@ local] codomain) means the flag
      reached that position: the closure was exempted.
    - "The allocation is "alloc" ... expected to be "noalloc_strict"" means the
      flag did NOT reach that position (a propagation gap): the closure counts as
      a heap allocation and forces the enclosing function.

    [exclave_] is used as a uniform wrapper so a returned [local] closure is
    allowed to escape its region; it does NOT by itself skip the allocation
    lock-walk (that is what [strictly_local_closure] does), so acceptance below
    genuinely reflects propagation -- see the control at the end of Part B. *)

(* --- Part A: positions that DO receive the flag (closure exempted) --- *)

(* Inner function parameter / curried closure (no [exclave_] needed). *)
let (p_curried @ noalloc_strict) x y = x
[%%expect{|
val p_curried : 'a -> ('b -> 'a) @ local = <fun>
|}]

(* Function body / return via [exclave_]. *)
let (p_exclave @ noalloc_strict) x = exclave_ (fun y -> x)
[%%expect{|
val p_exclave : 'a -> ('b -> 'a) @ local = <fun>
|}]

(* Mode annotation on the returned closure ([mode_morph]/[type_expect_mode]). We
   annotate a NON-locality axis ([contended]) on purpose: the [contended] in the
   result shows the annotation itself took effect (so the [mode_morph] path is
   exercised), while the [@ local] codomain is enforced by annotating the outer
   closure p_mode_annot with [@ noalloc_strict]. *)
let (p_mode_annot @ noalloc_strict) x = exclave_ (fun y -> x : _ @ contended)
[%%expect{|
val p_mode_annot : 'a -> ('b -> 'a) @ local contended = <fun>
|}]

(* Type constraint on the returned closure ([type_argument] reuses the expected
   mode). *)
let (p_constraint @ noalloc_strict) x = exclave_ ((fun y -> x) : _ -> _)
[%%expect{|
val p_constraint : 'a -> ('b -> 'a) @ local = <fun>
|}]

(* [if] branches (reuse the expected mode). *)
let (p_if @ noalloc_strict) b x =
  exclave_ (if b then (fun y -> x) else (fun y -> x))
[%%expect{|
val p_if : bool -> ('a -> 'b -> 'a) @ local = <fun>
|}]

(* [match] branches (reuse the expected mode). *)
let (p_match @ noalloc_strict) o x =
  exclave_ (match o with Some _ -> (fun y -> x) | None -> (fun y -> x))
[%%expect{|
val p_match : 'a option -> ('b -> 'c -> 'b) @ local = <fun>
|}]

(* Record field via a modality ([mode_is_contained_by]); the record block is
   [stack_]-allocated so the field closure is the only thing exercising the
   propagation. *)
type box = { g : unit -> int }
[%%expect{|
type box = { g : unit -> int; }
|}]
let (p_record @ noalloc_strict) (x : int) = exclave_ stack_ { g = (fun () -> x) }
[%%expect{|
val p_record : int -> box @ local = <fun>
|}]

(* --- Part B: positions that do NOT yet receive the flag (propagation gaps) --- *)

(* [let]-binding RHS: [pat_modes] rebuilds the RHS mode with [mode_default] and
   drops the flag. The closure is treated as a heap allocation. *)
let (g_let @ noalloc_strict) (x : int) =
  let h = fun () -> x in
  h ()
[%%expect{|
Line 2, characters 10-21:
2 |   let h = fun () -> x in
              ^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at lines 1-3, characters 29-6
         which is expected to be "noalloc_strict".
|}]

(* Application argument: [mode_argument] returns [mode_default] without the
   flag, so the argument closure is not exempted. [use] and [g_arg] are wrapped
   in an outer function so [use] stays a local [noalloc_strict] value -- a
   top-level [use] would instead be exported at [alloc] and trip the capture rule
   first. The only allocation here is the argument closure [fun () -> x], which
   is what the error correctly points at. *)
let g_app_arg () =
  let (use @ noalloc_strict) (g : unit -> int) = g () in
  let (g_arg @ noalloc_strict) (x : int) = use (fun () -> x) in
  ()
[%%expect{|
Line 3, characters 47-60:
3 |   let (g_arg @ noalloc_strict) (x : int) = use (fun () -> x) in
                                                   ^^^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 3, characters 31-60
         which is expected to be "noalloc_strict".
|}]

(* Region body ([for]/[while]/comprehension): [mode_region] rebuilds the body
   mode with [mode_default] and drops the flag. *)
let (g_region @ noalloc_strict) (x : int) =
  for _i = 0 to 0 do
    let _g = fun () -> x in ()
  done
[%%expect{|
Line 3, characters 13-24:
3 |     let _g = fun () -> x in ()
                 ^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at lines 1-4, characters 32-6
         which is expected to be "noalloc_strict".
|}]

(* Let-binding operator (letop): the [let*] body is a continuation closure typed
   at legacy via [mode_return] in [type_binding_op] (the [mode_return Value.legacy]
   there), so it does not receive the flag. In practice the failure surfaces even
   earlier: the [let*] operator is itself an [alloc] value, so referencing it
   inside a [noalloc_strict] function trips the capture rule first. Either way,
   [let*] does not currently compose with [noalloc_strict]. *)
let ( let* ) o f = match o with None -> None | Some x -> f x
[%%expect{|
val ( let* ) : 'a option -> ('a -> 'b option) -> 'b option = <fun>
|}]
let (g_letop @ noalloc_strict) (x : int) =
  let* _ = Some 0 in
  fun () -> x
[%%expect{|
Line 2, characters 2-6:
2 |   let* _ = Some 0 in
      ^^^^
Error: The value "( let* )" is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at lines 1-3, characters 31-13
         which is expected to be "noalloc_strict".
|}]

(* Control: [exclave_] does NOT rescue a gap position -- it forces [local] but
   does not skip the lock-walk, so the [let]-RHS closure still errors. This
   confirms the acceptances in Part A are due to [strictly_local_closure]
   propagation, not to [exclave_]. *)
let (g_let_exclave @ noalloc_strict) (x : int) = exclave_ (let h = fun () -> x in h)
[%%expect{|
Line 1, characters 67-78:
1 | let (g_let_exclave @ noalloc_strict) (x : int) = exclave_ (let h = fun () -> x in h)
                                                                       ^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 37-84
         which is expected to be "noalloc_strict".
|}]

(** Test 7: Misc *)

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

(* f triggers allocation, so it cannot be accepted as [@ noalloc_strict] *)
module M : sig val f : unit -> record_t @ noalloc_strict end = struct
    let (f @ noalloc_strict) () = { x = 3.0; y = 4.0 }
end
[%%expect{|
Line 2, characters 34-54:
2 |     let (f @ noalloc_strict) () = { x = 3.0; y = 4.0 }
                                      ^^^^^^^^^^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 2, characters 29-54
         which is expected to be "noalloc_strict".
|}]

(* Parameter is not captured *)
let alloc_para () =
  let h (g : (unit -> int) @ alloc) = g in
  (h : _ @ noalloc_strict)
[%%expect{|
val alloc_para : unit -> (unit -> int) -> unit -> int = <fun>
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

(* An object is always constructed at the legacy ([alloc]) mode, so it cannot be
   created at [noalloc_strict]: the binding annotation requires the object value
   to be [noalloc_strict], but its construction forces it to [alloc]. *)
let (o @ noalloc_strict) = object method get = 0 end
[%%expect{|
Line 1, characters 27-52:
1 | let (o @ noalloc_strict) = object method get = 0 end
                               ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "alloc" but is expected to be "noalloc_strict".
|}]

(** CR-soon shsong: revisit the following tests after supporting
    using zero_alloc backend for our analysis. *)
let (secretly_allocates @ noalloc) () =
  let (whitewashed @ alloc) = fun () -> { x = 1.; y = 2. } in
  whitewashed ()
[%%expect{|
Line 2, characters 30-58:
2 |   let (whitewashed @ alloc) = fun () -> { x = 1.; y = 2. } in
                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at lines 1-3, characters 35-16
         which is expected to be "noalloc".
|}]

let (secretly_allocates' @ noalloc) () = exclave_
 let mutable escape_hatch = None in
 (escape_hatch <- stack_ Some (fun () -> { x = 1.; y = 2. })) [@zero_alloc];
 match escape_hatch with
 | None -> stack_ { x = 1.; y = 2. }
 | Some f -> f ()
[%%expect{|
Line 3, characters 30-60:
3 |  (escape_hatch <- stack_ Some (fun () -> { x = 1.; y = 2. })) [@zero_alloc];
                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at lines 1-6, characters 36-17
         which is expected to be "noalloc".
|}]

let (secretly_allocates @ noalloc) () = exclave_
 let mutable (escape_hatch @ alloc) = None in
 (escape_hatch <- stack_ Some (fun () -> { x = 1.; y = 2. })) [@zero_alloc];
 match escape_hatch with
 | None -> stack_ { x = 1.; y = 2. }
 | Some f -> f ()
[%%expect{|
Line 3, characters 30-60:
3 |  (escape_hatch <- stack_ Some (fun () -> { x = 1.; y = 2. })) [@zero_alloc];
                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at lines 1-6, characters 35-17
         which is expected to be "noalloc".
|}]

(* [Pexp_lazy] (typecore.ml) must call [walk_locks_for_allocation] for the lazy
   *cell* BEFORE adding the [Lazy] closure lock for the thunk:

     Env.walk_locks_for_allocation ~env (loc, Hint.Allocation);       (* correct *)
     let env = Env.add_closure_lock (loc, Lazy) ... env in

   The cell is allocated in the *enclosing* scope, not inside the thunk, so the
   walk must use the enclosing env. If the walk is placed AFTER [add_closure_lock]
   (using the env that already contains the thunk's own [Lazy] lock), the cell
   allocation is wrongly attributed to the thunk.

   This top-level binding distinguishes the two placements:
   - correct placement: there is no enclosing closure to force, so it is
     accepted;
   - wrong placement: the walk traverses the thunk's [Lazy] lock and reports
     "...because it is used inside the lazy expression...". *)
let (lz_toplevel @ noalloc_strict) = lazy 1
[%%expect{|
val lz_toplevel : int lazy_t = lazy 1
|}]

(* For contrast: inside a function the two placements agree, because the cell
   walk reaches the enclosing function either way. *)
let (lz_in_fn @ noalloc_strict) (x : int) = lazy x
[%%expect{|
Line 1, characters 44-50:
1 | let (lz_in_fn @ noalloc_strict) (x : int) = lazy x
                                                ^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 32-50
         which is expected to be "noalloc_strict".
|}]
