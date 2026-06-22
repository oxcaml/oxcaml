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


(** Test 5: Allocation mode identifies all allocations and forces the proper
    mode. For each kind of allocation the typechecker can see, we test a
    [noalloc_strict] and a [noalloc] enclosing function side by side, and -- for
    the allocations that [stack_] supports -- a [stack_]-marked pair as well. An
    allocation currently forces the enclosing function to [alloc], which is
    above both [noalloc] and [noalloc_strict], so the detected cases error for
    both; the gaps (marked [CR shsong]) are accepted for both.

    CR shsong: [stack_] handling for the allocation axis is not yet implemented,
    so a [stack_]-marked allocation still forces [alloc] and errors just like a
    heap allocation. Once [stack_] is recognized, the [stack_]-marked cases
    below should be accepted -- a stack allocation does not count towards the
    allocation axis. (Allocations that [stack_] does not support, and [stack_]
    on non-allocations, are covered in the negative tests after this section.) *)

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
Line 1, characters 57-75:
1 | let (alloc_record @ noalloc_strict) () = exclave_ stack_ { x = 1.; y = 2. }
                                                             ^^^^^^^^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 36-75
         which is expected to be "noalloc_strict".
|}]
let (alloc_record @ noalloc) () = exclave_ stack_ { x = 1.; y = 2. }
[%%expect{|
Line 1, characters 50-68:
1 | let (alloc_record @ noalloc) () = exclave_ stack_ { x = 1.; y = 2. }
                                                      ^^^^^^^^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at line 1, characters 29-68
         which is expected to be "noalloc".
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
Line 1, characters 65-73:
1 | let (alloc_variant @ noalloc_strict) (a : int) = exclave_ stack_ (Just a)
                                                                     ^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 37-73
         which is expected to be "noalloc_strict".
|}]
let (alloc_variant @ noalloc) (a : int) = exclave_ stack_ (Just a)
[%%expect{|
Line 1, characters 58-66:
1 | let (alloc_variant @ noalloc) (a : int) = exclave_ stack_ (Just a)
                                                              ^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at line 1, characters 30-66
         which is expected to be "noalloc".
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
Line 1, characters 62-65:
1 | let (alloc_list @ noalloc_strict) (a : int) = exclave_ stack_ [a]
                                                                  ^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 34-65
         which is expected to be "noalloc_strict".
|}]
let (alloc_list @ noalloc) (a : int) = exclave_ stack_ [a]
[%%expect{|
Line 1, characters 55-58:
1 | let (alloc_list @ noalloc) (a : int) = exclave_ stack_ [a]
                                                           ^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at line 1, characters 27-58
         which is expected to be "noalloc".
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
Line 1, characters 63-70:
1 | let (alloc_array @ noalloc_strict) (a : int) = exclave_ stack_ [| a |]
                                                                   ^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 35-70
         which is expected to be "noalloc_strict".
|}]
let (alloc_array @ noalloc) (a : int) = exclave_ stack_ [| a |]
[%%expect{|
Line 1, characters 56-63:
1 | let (alloc_array @ noalloc) (a : int) = exclave_ stack_ [| a |]
                                                            ^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at line 1, characters 28-63
         which is expected to be "noalloc".
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
Line 1, characters 69-77:
1 | let (alloc_polyvariant @ noalloc_strict) (a : int) = exclave_ stack_ (`Tag a)
                                                                         ^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 41-77
         which is expected to be "noalloc_strict".
|}]
let (alloc_polyvariant @ noalloc) (a : int) = exclave_ stack_ (`Tag a)
[%%expect{|
Line 1, characters 62-70:
1 | let (alloc_polyvariant @ noalloc) (a : int) = exclave_ stack_ (`Tag a)
                                                                  ^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at line 1, characters 34-70
         which is expected to be "noalloc".
|}]

(* A function that takes an optional argument allocates (to handle the
   optional-argument wrapping). *)
let (alloc_optional_arg @ noalloc_strict) ?a () = a
[%%expect{|
Line 1, characters 45-51:
1 | let (alloc_optional_arg @ noalloc_strict) ?a () = a
                                                 ^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 42-51
         which is expected to be "noalloc_strict".
|}]
let (alloc_optional_arg @ noalloc) ?a () = a
[%%expect{|
Line 1, characters 38-44:
1 | let (alloc_optional_arg @ noalloc) ?a () = a
                                          ^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at line 1, characters 35-44
         which is expected to be "noalloc".
|}]

(* Building a closure that captures a variable allocates. *)
let (alloc_closure @ noalloc_strict) (a : int) = fun () -> a
[%%expect{|
Line 1, characters 49-60:
1 | let (alloc_closure @ noalloc_strict) (a : int) = fun () -> a
                                                     ^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 37-60
         which is expected to be "noalloc_strict".
|}]
let (alloc_closure @ noalloc) (a : int) = fun () -> a
[%%expect{|
Line 1, characters 42-53:
1 | let (alloc_closure @ noalloc) (a : int) = fun () -> a
                                              ^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at line 1, characters 30-53
         which is expected to be "noalloc".
|}]

(* The same closure, but [stack_]-marked. *)
let (alloc_closure @ noalloc_strict) (a : int) = exclave_ stack_ (fun () -> a)
[%%expect{|
Line 1, characters 65-78:
1 | let (alloc_closure @ noalloc_strict) (a : int) = exclave_ stack_ (fun () -> a)
                                                                     ^^^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 37-78
         which is expected to be "noalloc_strict".
|}]
let (alloc_closure @ noalloc) (a : int) = exclave_ stack_ (fun () -> a)
[%%expect{|
Line 1, characters 58-71:
1 | let (alloc_closure @ noalloc) (a : int) = exclave_ stack_ (fun () -> a)
                                                              ^^^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at line 1, characters 30-71
         which is expected to be "noalloc".
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
Line 1, characters 76-79:
1 | let (alloc_float_boxing @ noalloc_strict) (r : record_t5) = exclave_ stack_ r.x
                                                                                ^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 42-79
         which is expected to be "noalloc_strict".
|}]
let (alloc_float_boxing @ noalloc) (r : record_t5) = exclave_ stack_ r.x
[%%expect{|
Line 1, characters 69-72:
1 | let (alloc_float_boxing @ noalloc) (r : record_t5) = exclave_ stack_ r.x
                                                                         ^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at line 1, characters 35-72
         which is expected to be "noalloc".
|}]

(* CR shsong: tuple construction allocates a boxed block, but the allocation
   is registered via [register_allocation_value_mode], which does not walk
   locks, so this is wrongly accepted. (We use a unit argument and constant
   elements to avoid an inner currying closure, which would otherwise be the
   real cause of an error.) *)
let (alloc_tuple @ noalloc_strict) () = (1, 2)
[%%expect{|
val alloc_tuple : unit -> int * int = <fun>
|}]
let (alloc_tuple @ noalloc) () = (1, 2)
[%%expect{|
val alloc_tuple : unit -> int * int = <fun>
|}]

(* CR shsong: the same tuple, but [stack_]-marked. Tuples are a gap, so this
   is accepted regardless of [stack_]. *)
let (alloc_tuple @ noalloc_strict) () = exclave_ stack_ (1, 2)
[%%expect{|
val alloc_tuple : unit -> int * int @ local = <fun>
|}]
let (alloc_tuple @ noalloc) () = exclave_ stack_ (1, 2)
[%%expect{|
val alloc_tuple : unit -> int * int @ local = <fun>
|}]

(* CR shsong: partial application allocates a closure, but it is registered
   via [register_allocation_mode], which does not walk locks, so this is
   wrongly accepted. We receive [f] as a parameter (rather than defining a
   multi-argument function, whose curried closures would themselves be flagged)
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

(* CR shsong: a lazy block allocates on the heap, but no allocation is
   registered for it, so this is wrongly accepted. We use [lazy a] with
   [a : int] so the thunk body has no [alloc] reference. *)
let (alloc_lazy @ noalloc_strict) (a : int) = lazy a
[%%expect{|
val alloc_lazy : int -> int lazy_t = <fun>
|}]
let (alloc_lazy @ noalloc) (a : int) = lazy a
[%%expect{|
val alloc_lazy : int -> int lazy_t = <fun>
|}]

(* An object allocates. *)
let (alloc_object @ noalloc_strict) () = object method m = 1 end
[%%expect{|
Line 1, characters 59-60:
1 | let (alloc_object @ noalloc_strict) () = object method m = 1 end
                                                               ^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 36-64
         which is expected to be "noalloc_strict".
|}]
let (alloc_object @ noalloc) () = object method m = 1 end
[%%expect{|
Line 1, characters 52-53:
1 | let (alloc_object @ noalloc) () = object method m = 1 end
                                                        ^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at line 1, characters 29-57
         which is expected to be "noalloc".
|}]

(* CR shsong: packing a first-class module [(module M)] allocates, but no
   allocation is registered for it ([Pexp_pack] in [type_expect] only submodes;
   the module-level [register_allocation] in [typemod.ml] does not walk locks),
   so this is wrongly accepted. *)
module type S = sig end
module Empty : S = struct end
[%%expect{|
module type S = sig end
module Empty : S @@ stateless noalloc_strict
|}]
let (alloc_first_class_module @ noalloc_strict) () = (module Empty : S)
[%%expect{|
val alloc_first_class_module : unit -> (module S) = <fun>
|}]
let (alloc_first_class_module @ noalloc) () = (module Empty : S)
[%%expect{|
val alloc_first_class_module : unit -> (module S) = <fun>
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
Line 1, characters 70-81:
1 | let (alloc_exception @ noalloc_strict) (a : string) = exclave_ stack_ (Failure a)
                                                                          ^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 39-81
         which is expected to be "noalloc_strict".
|}]
let (alloc_exception @ noalloc) (a : string) = exclave_ stack_ (Failure a)
[%%expect{|
Line 1, characters 63-74:
1 | let (alloc_exception @ noalloc) (a : string) = exclave_ stack_ (Failure a)
                                                                   ^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at line 1, characters 32-74
         which is expected to be "noalloc".
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

(* CR shsong: a list/array comprehension allocates its result, but no
   allocation is registered for it (the comprehension result block is built
   directly at [Value.legacy]), so this is wrongly accepted. *)
let (alloc_list_comprehension @ noalloc_strict) () = [ x for x = 1 to 10 ]
[%%expect{|
val alloc_list_comprehension : unit -> int list = <fun>
|}]
let (alloc_list_comprehension @ noalloc) () = [ x for x = 1 to 10 ]
[%%expect{|
val alloc_list_comprehension : unit -> int list = <fun>
|}]


(** Test 5b: [stack_] negative cases *)

(* [stack_] on a non-allocation (a plain value) is rejected: it is not an
   allocation. *)
let (stack_non_alloc @ noalloc_strict) (a : int) = exclave_ stack_ a
[%%expect{|
Line 5, characters 67-68:
5 | let (stack_non_alloc @ noalloc_strict) (a : int) = exclave_ stack_ a
                                                                       ^
Error: This expression is not an allocation site.
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

(* [stack_] does not support lazy expressions. *)
let (stack_lazy @ noalloc_strict) (a : int) = exclave_ stack_ (lazy a)
[%%expect{|
Line 1, characters 62-70:
1 | let (stack_lazy @ noalloc_strict) (a : int) = exclave_ stack_ (lazy a)
                                                                  ^^^^^^^^
Error: Stack allocating lazy expressions is unsupported yet.
|}]

(* [stack_] does not support objects; here the object's method closure is
   itself a detected allocation, so the allocation-axis error fires on the
   method body before the unsupported-[stack_] check is reached. *)
let (stack_object @ noalloc_strict) () = exclave_ stack_ (object method m = 1 end)
[%%expect{|
Line 1, characters 76-77:
1 | let (stack_object @ noalloc_strict) () = exclave_ stack_ (object method m = 1 end)
                                                                                ^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 36-82
         which is expected to be "noalloc_strict".
|}]

(* [stack_] does not support first-class modules ([Empty]/[S] from Test 5). *)
let (stack_first_class_module @ noalloc_strict) () = exclave_ stack_ (module Empty : S)
[%%expect{|
Line 1, characters 69-87:
1 | let (stack_first_class_module @ noalloc_strict) () = exclave_ stack_ (module Empty : S)
                                                                         ^^^^^^^^^^^^^^^^^^
Error: Stack allocating modules is unsupported yet.
|}]

(* [stack_] does not support comprehensions. *)
let (stack_comprehension @ noalloc_strict) () = exclave_ stack_ [ x for x = 1 to 10 ]
[%%expect{|
Line 1, characters 64-85:
1 | let (stack_comprehension @ noalloc_strict) () = exclave_ stack_ [ x for x = 1 to 10 ]
                                                                    ^^^^^^^^^^^^^^^^^^^^^
Error: Stack allocating list comprehensions is unsupported yet.
|}]


(** Test 5b: [stack_] nested cases. *)
(* Nested: an inner allocation that is not itself [stack_]-marked still
   allocates on the heap, even when an enclosing allocation is [stack_]-marked.
   (Currently the outer [stack_] is not recognized either, so both contribute;
   once [stack_] is recognized, only the inner allocation should remain and the
   function should still be rejected.) *)
let (stack_nested @ noalloc_strict) (a : int) = exclave_ stack_ (Just (Just a))
[%%expect{|
Line 1, characters 64-79:
1 | let (stack_nested @ noalloc_strict) (a : int) = exclave_ stack_ (Just (Just a))
                                                                    ^^^^^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 36-79
         which is expected to be "noalloc_strict".
|}]


(** Test 6: Other tests *)

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

(* CR-soon shsong: error expected -- cannot call [whitewashed ()] *)
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

(* CR-soon shsong: error expected -- cannot assign alloc func to escape_hatch ref *)
let (secretly_allocates' @ noalloc) () = exclave_
 let mutable escape_hatch = None in
 (escape_hatch <- stack_ Some (fun () -> { x = 1.; y = 2. })) [@zero_alloc];
 match escape_hatch with
 | None -> stack_ { x = 1.; y = 2. }
 | Some f -> f ()
[%%expect{|
Line 3, characters 25-60:
3 |  (escape_hatch <- stack_ Some (fun () -> { x = 1.; y = 2. })) [@zero_alloc];
                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at lines 1-6, characters 36-17
         which is expected to be "noalloc".
|}]

(* CR-soon shsong: error expected -- cannot call [f ()] because it is alloc *)
let (secretly_allocates @ noalloc) () = exclave_
 let mutable (escape_hatch @ alloc) = None in
 (escape_hatch <- stack_ Some (fun () -> { x = 1.; y = 2. })) [@zero_alloc];
 match escape_hatch with
 | None -> stack_ { x = 1.; y = 2. }
 | Some f -> f ()
[%%expect{|
Line 3, characters 25-60:
3 |  (escape_hatch <- stack_ Some (fun () -> { x = 1.; y = 2. })) [@zero_alloc];
                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The allocation is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at lines 1-6, characters 35-17
         which is expected to be "noalloc".
|}]
