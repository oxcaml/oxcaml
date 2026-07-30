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


(** Test 5.1: When a function is anntoated with [noalloc_strict]/[noalloc],
    Allocation mode identifies all allocations in the function and force them
    to be local. *)
(* CR shsong: Currently [noalloc_strict]/[noalloc] has the same requirement,
    both force all allocations to be local, so test results for them should be
    the same. We will add more tests to test difference between them after we
    make [noalloc] ignore allocation on exception branches. *)

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
Error: The allocation is "local"
         because it is inside the function at line 1, characters 36-59
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]
let (alloc_record @ noalloc) () = { x = 1.; y = 2. }
[%%expect{|
Line 1, characters 34-52:
1 | let (alloc_record @ noalloc) () = { x = 1.; y = 2. }
                                      ^^^^^^^^^^^^^^^^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 29-52
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

(* The same record allocation, but [exclave_]-marked. *)
let (alloc_record @ noalloc_strict) () = exclave_ { x = 1.; y = 2. }
[%%expect{|
val alloc_record : unit -> record_t5 @ local = <fun>
|}]
let (alloc_record @ noalloc) () = exclave_ { x = 1.; y = 2. }
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
Error: The allocation is "local"
         because it is inside the function at line 1, characters 37-55
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]
let (alloc_variant @ noalloc) (a : int) = Just a
[%%expect{|
Line 1, characters 42-48:
1 | let (alloc_variant @ noalloc) (a : int) = Just a
                                              ^^^^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 30-48
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

(* The same variant allocation, but [exclave_]-marked. *)
let (alloc_variant @ noalloc_strict) (a : int) = exclave_ (Just a)
[%%expect{|
val alloc_variant : int -> int variant_t5 @ local = <fun>
|}]
let (alloc_variant @ noalloc) (a : int) = exclave_ (Just a)
[%%expect{|
val alloc_variant : int -> int variant_t5 @ local = <fun>
|}]

(* A cons cell of a list allocates. *)
let (alloc_list @ noalloc_strict) (a : int) = [a]
[%%expect{|
Line 1, characters 46-49:
1 | let (alloc_list @ noalloc_strict) (a : int) = [a]
                                                  ^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 34-49
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]
let (alloc_list @ noalloc) (a : int) = [a]
[%%expect{|
Line 1, characters 39-42:
1 | let (alloc_list @ noalloc) (a : int) = [a]
                                           ^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 27-42
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

(* The same list cell, but [exclave_]-marked. *)
let (alloc_list @ noalloc_strict) (a : int) = exclave_ [a]
[%%expect{|
val alloc_list : int -> int list @ local = <fun>
|}]
let (alloc_list @ noalloc) (a : int) = exclave_ [a]
[%%expect{|
val alloc_list : int -> int list @ local = <fun>
|}]

(* An array literal allocates. *)
let (alloc_array @ noalloc_strict) (a : int) = [| a |]
[%%expect{|
Line 1, characters 47-54:
1 | let (alloc_array @ noalloc_strict) (a : int) = [| a |]
                                                   ^^^^^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 35-54
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]
let (alloc_array @ noalloc) (a : int) = [| a |]
[%%expect{|
Line 1, characters 40-47:
1 | let (alloc_array @ noalloc) (a : int) = [| a |]
                                            ^^^^^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 28-47
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

(* The same array, but [exclave_]-marked. *)
let (alloc_array @ noalloc_strict) (a : int) = exclave_ [| a |]
[%%expect{|
val alloc_array : int -> int array @ local = <fun>
|}]
let (alloc_array @ noalloc) (a : int) = exclave_ [| a |]
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
Error: The allocation is "local"
         because it is inside the function at line 1, characters 41-59
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]
let (alloc_polyvariant @ noalloc) (a : int) = `Tag a
[%%expect{|
Line 1, characters 46-52:
1 | let (alloc_polyvariant @ noalloc) (a : int) = `Tag a
                                                  ^^^^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 34-52
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

(* The same polymorphic variant, but [exclave_]-marked. *)
let (alloc_polyvariant @ noalloc_strict) (a : int) = exclave_ (`Tag a)
[%%expect{|
val alloc_polyvariant : int -> [> `Tag of int ] @ local = <fun>
|}]
let (alloc_polyvariant @ noalloc) (a : int) = exclave_ (`Tag a)
[%%expect{|
val alloc_polyvariant : int -> [> `Tag of int ] @ local = <fun>
|}]


(* A function that takes an optional argument can still be noalloc.
  Calling the function without providing the optional argument does not
  trigger allocation.
  Calling the function with providing the optional argument triggers
  allocation, which is successfully detected. *)
let (alloc_optional_arg @ noalloc_strict) ?a () = a
[%%expect{|
val alloc_optional_arg : ?a:'a -> (unit -> 'a option) @ local = <fun>
|}]
let (alloc_optional_arg @ noalloc) ?a () = a
[%%expect{|
val alloc_optional_arg : ?a:'a -> (unit -> 'a option) @ local = <fun>
|}]

let test_missing_arg () =
  let (f @ noalloc_strict) ?a () = a in
  let (g @ noalloc_strict) () = f () in
  g ()
[%%expect{|
val test_missing_arg : unit -> 'a option = <fun>
|}]

let test_passing_arg () =
  let (f @ noalloc_strict) ?a () = a in
  let (g @ noalloc_strict) () = f ~a:1 () in
  g ()
[%%expect{|
Line 3, characters 37-38:
3 |   let (g @ noalloc_strict) () = f ~a:1 () in
                                         ^
Error: The allocation is "local"
         because it is inside the function at line 3, characters 27-41
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

let test_missing_arg () =
  let (f @ noalloc_strict) ?(a=0) () = a in
  let (g @ noalloc_strict) () = f () in
  g ()
[%%expect{|
val test_missing_arg : unit -> int = <fun>
|}]

let test_passing_arg () =
  let (f @ noalloc_strict) ?(a=0) () = a in
  let (g @ noalloc_strict) () = f ~a:1 () in
  g ()
[%%expect{|
Line 3, characters 37-38:
3 |   let (g @ noalloc_strict) () = f ~a:1 () in
                                         ^
Error: The allocation is "local"
         because it is inside the function at line 3, characters 27-41
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

(* Building a closure inside a noalloc function forcing the closure
  to be local. *)
let (alloc_closure @ noalloc_strict) (a : int) = fun () -> a
[%%expect{|
Line 1, characters 49-60:
1 | let (alloc_closure @ noalloc_strict) (a : int) = fun () -> a
                                                     ^^^^^^^^^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 37-60
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global"
         because it is an allocation
         which is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]
let (alloc_closure @ noalloc) (a : int) = fun () -> a
[%%expect{|
Line 1, characters 42-53:
1 | let (alloc_closure @ noalloc) (a : int) = fun () -> a
                                              ^^^^^^^^^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 30-53
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global"
         because it is an allocation
         which is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

(* The same closure, but [exclave_]-marked. *)
let (alloc_closure @ noalloc_strict) (a : int) = exclave_ (fun () -> a)
[%%expect{|
val alloc_closure : int -> (unit -> int) @ local = <fun>
|}]
let (alloc_closure @ noalloc) (a : int) = exclave_ (fun () -> a)
[%%expect{|
val alloc_closure : int -> (unit -> int) @ local = <fun>
|}]

(* Boxing a float read out of a flat float record allocates. *)
let (alloc_float_boxing @ noalloc_strict) (r : record_t5) = r.x
[%%expect{|
Line 1, characters 60-63:
1 | let (alloc_float_boxing @ noalloc_strict) (r : record_t5) = r.x
                                                                ^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 42-63
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]
let (alloc_float_boxing @ noalloc) (r : record_t5) = r.x
[%%expect{|
Line 1, characters 53-56:
1 | let (alloc_float_boxing @ noalloc) (r : record_t5) = r.x
                                                         ^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 35-56
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

(* The same float boxing, but [exclave_]-marked. *)
let (alloc_float_boxing @ noalloc_strict) (r : record_t5) = exclave_ r.x
[%%expect{|
val alloc_float_boxing : record_t5 -> float @ local = <fun>
|}]
let (alloc_float_boxing @ noalloc) (r : record_t5) = exclave_ r.x
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
Error: The allocation is "local"
         because it is inside the function at line 1, characters 35-46
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]
let (alloc_tuple @ noalloc) () = (1, 2)
[%%expect{|
Line 1, characters 33-39:
1 | let (alloc_tuple @ noalloc) () = (1, 2)
                                     ^^^^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 28-39
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

(* The same tuple, but [exclave_]-marked. *)
let (alloc_tuple @ noalloc_strict) () = exclave_ (1, 2)
[%%expect{|
val alloc_tuple : unit -> int * int @ local = <fun>
|}]
let (alloc_tuple @ noalloc) () = exclave_ (1, 2)
[%%expect{|
val alloc_tuple : unit -> int * int @ local = <fun>
|}]

(* We do not check partial application to detect potential allocation triggered
  by it. Instead, we guarantee that if a function is noalloc, then
  calling it with partial application does not trigger any heap allocation. *)
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

(* Partial application whose result is a function hidden behind a
   type abbreviation ([myfun = int -> int]). *)
type myfun = int -> int
let (alloc_partial_app_abbrev @ noalloc_strict)
      (f : (int -> int -> myfun) @ noalloc_strict) = f 1 2
[%%expect{|
type myfun = int -> int
val alloc_partial_app_abbrev :
  (int -> int -> myfun) @ noalloc_strict -> myfun = <fun>
|}]

(* At function call site, we only detect allocation by passing an optional
  argument, but do not detect potential allocation by partial application.

  A function [f] with mixed arguments: optional [a], mandatory [b], optional
  [c], mandatory [d], optional [e], mandatory [g]. It ends in a mandatory
  argument [g], so it can be fully applied. It is received as a parameter so
  referencing it (in function position) does not itself trigger the capture
  rule; the only allocations come from how it is called. *)

(* Case 1: call with all mandatory args ([b], [d], [g]); all optionals are
  omitted, so they default to [None] (constant constructors, no [Some]
  wrapping). Furthermore, [f] is fully applied. Thus, it is accepted. *)
let (call_all_mandatory @ noalloc_strict)
      (f : (?a:int -> int -> ?c:int -> int -> ?e:int -> int -> int)) =
  f 1 2 3
[%%expect{|
val call_all_mandatory :
  (?a:int -> int -> ?c:int -> int -> ?e:int -> int -> int) -> int = <fun>
|}]

(* Case 2: call with two mandatory args ([b], [d]); all optionals are
  omitted, so they default to [None] (constant constructors, no [Some]
  wrapping). One mandatory arg is missing. Note that [call_two_mand]
  does not allocate as long as [f] is noalloc, since our typing rule
  guarantee that noalloc function does not allocate even when partially
  applied. Thus, call_two_mand can be noalloc. *)
let (call_two_mand @ noalloc_strict)
      (f : (?a:int -> int -> ?c:int -> int -> ?e:int -> int -> int)) =
  f 1 2
[%%expect{|
val call_two_mand :
  (?a:int -> int -> ?c:int -> int -> ?e:int -> int -> int) ->
  ?e:int -> int -> int = <fun>
|}]

(* Case 3: call with one optional ([~a:1]) and one mandatory ([b]). The provided
  [~a:1] would be wrapped in [Some], which triggers allocation. Thus, it is
  rejected. *)
let (call_one_opt_one_mand @ noalloc_strict)
      (f : (?a:int -> int -> ?c:int -> int -> ?e:int -> int -> int)) =
  f ~a:1 2
[%%expect{|
Line 3, characters 7-8:
3 |   f ~a:1 2
           ^
Error: The allocation is "local"
         because it is inside the function at lines 2-3, characters 6-10
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

(* A partial application that omits an earlier labeled argument (here [~x], while
    the later [~y] is given) forces the compiler to abstract over the omitted
    parameter, allocating a closure in
    [type_omitted_parameters_and_build_result_type].

    This is a special case for closure allocation at partial application where we
    force the closure allocation to be local at callee's call site, since
    the compiler explicitly register allocation for it.
    However, for other closure allocation at partial application, noalloc forces
    the allocation to be local at the callee's definition site, since whether
    there would be allocation at the call site might be unknown at typing stage. *)
let partial_application_omit_earlier_arg (h : (x:int -> y:int -> int)) =
  let (require_noalloc @ noalloc_strict) () =
    (h ~y:1)
  in
  let _ = require_noalloc () in
  ()
[%%expect{|
Line 5, characters 10-28:
5 |   let _ = require_noalloc () in
              ^^^^^^^^^^^^^^^^^^
Warning 5 [ignored-partial-application]: this function application is partial,
  maybe some arguments are missing.

Line 3, characters 4-12:
3 |     (h ~y:1)
        ^^^^^^^^
Error: The allocation is "local"
         because it is inside the function at lines 2-3, characters 41-12
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

let partial_application_omit_earlier_arg (h : (x:int -> y:int -> int)) =
  let (require_noalloc @ noalloc_strict) () = exclave_
    (h ~y:1)
  in
  let _ = require_noalloc () in
  ()
[%%expect{|
Line 5, characters 10-28:
5 |   let _ = require_noalloc () in
              ^^^^^^^^^^^^^^^^^^
Warning 5 [ignored-partial-application]: this function application is partial,
  maybe some arguments are missing.

val partial_application_omit_earlier_arg :
  (x:int -> y:int -> int) @ noalloc_strict -> unit = <fun>
|}]

(* A lazy block always allocates on the heap and cannot be made [local]. *)
let (alloc_lazy @ noalloc_strict) (a : int) = let _ = lazy a in ()
[%%expect{|
Line 1, characters 54-60:
1 | let (alloc_lazy @ noalloc_strict) (a : int) = let _ = lazy a in ()
                                                          ^^^^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 34-66
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]
let (alloc_lazy @ noalloc) (a : int) =  let _ = lazy a in ()
[%%expect{|
Line 1, characters 48-54:
1 | let (alloc_lazy @ noalloc) (a : int) =  let _ = lazy a in ()
                                                    ^^^^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 27-60
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

let (alloc_lazy @ noalloc) (a : int) = exclave_ (lazy a)
[%%expect{|
Line 1, characters 48-56:
1 | let (alloc_lazy @ noalloc) (a : int) = exclave_ (lazy a)
                                                    ^^^^^^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 27-56
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

(* An object always allocates on the heap and cannot be made [local]. *)
let (alloc_object @ noalloc_strict) () =
  let _ = object method m = 1 end in ()
[%%expect{|
Line 2, characters 10-33:
2 |   let _ = object method m = 1 end in ()
              ^^^^^^^^^^^^^^^^^^^^^^^
Error: The allocation is "local"
         because it is inside the function at lines 1-2, characters 36-39
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]
let (alloc_object @ noalloc) () =
  let _ = object method m = 1 end in ()
[%%expect{|
Line 2, characters 10-33:
2 |   let _ = object method m = 1 end in ()
              ^^^^^^^^^^^^^^^^^^^^^^^
Error: The allocation is "local"
         because it is inside the function at lines 1-2, characters 29-39
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

let (stack_object @ noalloc_strict) () = exclave_ (object method m = 1 end)
[%%expect{|
Line 1, characters 50-75:
1 | let (stack_object @ noalloc_strict) () = exclave_ (object method m = 1 end)
                                                      ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 36-75
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

(* An object with no allocating method (here only a value field) still
   allocates the object block itself. *)
let (obj_no_alloc_method @ noalloc_strict) () =
  let _ = object val x = 0 end in ()
[%%expect{|
Line 2, characters 10-30:
2 |   let _ = object val x = 0 end in ()
              ^^^^^^^^^^^^^^^^^^^^
Error: The allocation is "local"
         because it is inside the function at lines 1-2, characters 43-36
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

let (obj_no_alloc_method @ noalloc) () =
  let _ = object val x = 0 end in ()
[%%expect{|
Line 2, characters 10-30:
2 |   let _ = object val x = 0 end in ()
              ^^^^^^^^^^^^^^^^^^^^
Error: The allocation is "local"
         because it is inside the function at lines 1-2, characters 36-36
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

let (obj_no_alloc_method @ noalloc_strict) () = exclave_ object val x = 0 end
[%%expect{|
Line 1, characters 57-77:
1 | let (obj_no_alloc_method @ noalloc_strict) () = exclave_ object val x = 0 end
                                                             ^^^^^^^^^^^^^^^^^^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 43-77
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

(* A functional-update override [{< ... >}] copies [self], which allocates.
   Like an [object] block, the copy is registered as an allocation, so it is
   forced [local] inside a noalloc function; since the object copy needs
   [global], it is rejected. *)
let f () =
  object (self)
    val y = 0
    method bump =
      let (require_noalloc @ noalloc_strict) () =
        let _ = {< y = 1 >} in ()
      in
      require_noalloc ()
  end
[%%expect{|
Line 6, characters 16-27:
6 |         let _ = {< y = 1 >} in ()
                    ^^^^^^^^^^^
Error: The allocation is "local"
         because it is inside the function at lines 5-6, characters 45-33
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]


(* Packing a first-class module [(module M)] allocates *)
module type S = sig end
module Empty : S = struct end
[%%expect{|
module type S = sig end
module Empty : S @@ stateless noalloc_strict
|}]
let (alloc_first_class_module @ noalloc_strict) () = let _ = (module Empty : S) in ()
[%%expect{|
Line 1, characters 69-74:
1 | let (alloc_first_class_module @ noalloc_strict) () = let _ = (module Empty : S) in ()
                                                                         ^^^^^
Error: The module is "alloc" because it is allocated on the heap.
       However, the module highlighted is expected to be "noalloc_strict"
         because it is used inside the function at line 1, characters 48-85
         which is expected to be "noalloc_strict".
|}]
let (alloc_first_class_module @ noalloc) () = let _ = (module Empty : S) in ()
[%%expect{|
Line 1, characters 62-67:
1 | let (alloc_first_class_module @ noalloc) () = let _ = (module Empty : S) in ()
                                                                  ^^^^^
Error: The module is "alloc" because it is allocated on the heap.
       However, the module highlighted is expected to be "noalloc"
         because it is used inside the function at line 1, characters 41-78
         which is expected to be "noalloc".
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
Error: The allocation is "local"
         because it is allocated inside the function at line 1, characters 38-61,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]
let (alloc_external @ noalloc) (a : int) = my_extern a
[%%expect{|
Line 1, characters 43-52:
1 | let (alloc_external @ noalloc) (a : int) = my_extern a
                                               ^^^^^^^^^
Error: The allocation is "local"
         because it is allocated inside the function at line 1, characters 31-54,
         which is "noalloc" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

(* Boxed arithmetic really does allocate: [%addfloat] boxes its result. The
   result is [@local_opt], so the checker offers to stack-allocate the box,
   which is legitimate inside a [noalloc] function; here the box escapes as the
   return value, so the rejection surfaces as a regionality error rather than
   an allocation one. See [prim_poly_result] below for the same shape. *)
let (alloc_float_arith @ noalloc_strict) (a : float) = a +. a
[%%expect{|
Line 1, characters 55-61:
1 | let (alloc_float_arith @ noalloc_strict) (a : float) = a +. a
                                                           ^^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]
let (alloc_float_arith @ noalloc) (a : float) = a +. a
[%%expect{|
Line 1, characters 48-54:
1 | let (alloc_float_arith @ noalloc) (a : float) = a +. a
                                                    ^^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

(* Likewise for boxed-integer arithmetic via a [Stdlib] primitive. *)
let (alloc_int64_arith @ noalloc_strict) (a : int64) = Int64.add a a
[%%expect{|
Line 1, characters 55-68:
1 | let (alloc_int64_arith @ noalloc_strict) (a : int64) = Int64.add a a
                                                           ^^^^^^^^^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]
let (alloc_int64_arith @ noalloc) (a : int64) = Int64.add a a
[%%expect{|
Line 1, characters 48-61:
1 | let (alloc_int64_arith @ noalloc) (a : int64) = Int64.add a a
                                                    ^^^^^^^^^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

(* A fully-applied non-allocating [int] primitive does not force the enclosing
   function to [alloc], so this is accepted. *)
let (alloc_int_arith @ noalloc_strict) (a : int) = a + a
[%%expect{|
val alloc_int_arith : int -> int = <fun>
|}]
let (alloc_int_arith @ noalloc) (a : int) = a + a
[%%expect{|
val alloc_int_arith : int -> int = <fun>
|}]

(* Referencing [(+)] bare eta-expands it into a closure, which allocates. *)
let (alloc_int_ref @ noalloc_strict) () = ( + )
[%%expect{|
Line 1, characters 42-47:
1 | let (alloc_int_ref @ noalloc_strict) () = ( + )
                                              ^^^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 37-47
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]
let (alloc_int_ref @ noalloc) () = ( + )
[%%expect{|
Line 1, characters 35-40:
1 | let (alloc_int_ref @ noalloc) () = ( + )
                                       ^^^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 30-40
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

(* A partial application of [(+)] also allocates a closure. *)
let (alloc_int_partial @ noalloc_strict) (a : int) = ( + ) a
[%%expect{|
Line 1, characters 53-58:
1 | let (alloc_int_partial @ noalloc_strict) (a : int) = ( + ) a
                                                         ^^^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 41-60
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]
let (alloc_int_partial @ noalloc) (a : int) = ( + ) a
[%%expect{|
Line 1, characters 46-51:
1 | let (alloc_int_partial @ noalloc) (a : int) = ( + ) a
                                                  ^^^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 34-53
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

(* CR shsong: [( + )] is Prim_global, we do not have the corresponding
    mode variable in [type_ident], so we just register allocation with
    legacy mode [alloc], and cannot make its partial application result
    to local. *)
(* [(( + ) a) a] forces [( + ) a] to be materialised as its own partial
   application (a closure), unlike the fully-applied [a + a]. *)
let (alloc_int_paren @ noalloc_strict) (a : int) = (( + ) a) a
[%%expect{|
Line 1, characters 52-57:
1 | let (alloc_int_paren @ noalloc_strict) (a : int) = (( + ) a) a
                                                        ^^^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 39-62
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]
let (alloc_int_paren @ noalloc) (a : int) = (( + ) a) a
[%%expect{|
Line 1, characters 45-50:
1 | let (alloc_int_paren @ noalloc) (a : int) = (( + ) a) a
                                                 ^^^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 32-55
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

let (alloc_int_paren @ noalloc_strict) (a : int) = exclave_ (( + ) a) a
[%%expect{|
Line 1, characters 61-66:
1 | let (alloc_int_paren @ noalloc_strict) (a : int) = exclave_ (( + ) a) a
                                                                 ^^^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 39-71
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]
let (alloc_int_paren @ noalloc) (a : int) = exclave_ (( + ) a) a
[%%expect{|
Line 1, characters 54-59:
1 | let (alloc_int_paren @ noalloc) (a : int) = exclave_ (( + ) a) a
                                                          ^^^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 32-64
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

(* Primitive's mode is kept on aliasing, not modified by its fully-applied
   exemption. *)
let (f_alias @ noalloc_strict) = ( + )
[%%expect{|
Line 1, characters 33-38:
1 | let (f_alias @ noalloc_strict) = ( + )
                                     ^^^^^
Error: This value is "alloc" but is expected to be "noalloc_strict".
|}]

(* The fully-applied relaxation also reaches primitives applied through [|>]
   ([%revapply]) and [@@] ([%apply]). Both operators are themselves fully
   applied and non-allocating, and the actual function's args are threaded
   through as its own applied arity, so these fully-applied cases are accepted. *)

(* [|>] with a simple (non-application) head: [( ~- )] fully applied. *)
let (alloc_int_revapply @ noalloc_strict) (a : int) = a |> ( ~- )
[%%expect{|
val alloc_int_revapply : int -> int = <fun>
|}]

(* [|>] with an application head: [( + ) b] fully applied to [a] as well. *)
let (alloc_int_revapply_app @ noalloc_strict) (a : int) (b : int) =
  a |> ( + ) b
[%%expect{|
val alloc_int_revapply_app : int -> (int -> int) @ local = <fun>
|}]

let (alloc_int_add_revapply @ noalloc_strict) (a : int) =
  a |> (( + ) a)
[%%expect{|
val alloc_int_add_revapply : int -> int = <fun>
|}]

(* [@@] with a simple head: [( ~- )] fully applied. *)
let (alloc_int_apply @ noalloc_strict) (a : int) = ( ~- ) @@ a
[%%expect{|
val alloc_int_apply : int -> int = <fun>
|}]

(* [@@] with an application head: [( + ) a] fully applied to [b] as well. *)
let (alloc_int_apply_app @ noalloc_strict) (a : int) (b : int) =
  ( + ) a @@ b
[%%expect{|
val alloc_int_apply_app : int -> (int -> int) @ local = <fun>
|}]

let (alloc_int_add_apply @ noalloc_strict) (a : int) =
  (( + ) a) @@ a
[%%expect{|
val alloc_int_add_apply : int -> int = <fun>
|}]

(* Guard: a *partial* application through [|>] stays rejected -- [( + ) a] is a
   closure, so [( + )] is not fully applied and reports the [noalloc] error. *)
let (alloc_int_revapply_partial @ noalloc_strict) (a : int) = a |> ( + )
[%%expect{|
Line 1, characters 67-72:
1 | let (alloc_int_revapply_partial @ noalloc_strict) (a : int) = a |> ( + )
                                                                       ^^^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 50-72
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

let (alloc_int_apply_partial @ noalloc_strict) (a : int) = ( + ) @@ a
[%%expect{|
Line 1, characters 59-64:
1 | let (alloc_int_apply_partial @ noalloc_strict) (a : int) = ( + ) @@ a
                                                               ^^^^^
Error: The allocation is "local"
         because it is allocated inside the function at line 1, characters 47-69,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
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
Error: The allocation is "local"
         because it is inside the function at line 1, characters 39-63
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]
let (alloc_exception @ noalloc) (a : string) = Failure a
[%%expect{|
Line 1, characters 47-56:
1 | let (alloc_exception @ noalloc) (a : string) = Failure a
                                                   ^^^^^^^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 32-56
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

(* The same exception construction, but [exclave_]-marked. *)
let (alloc_exception @ noalloc_strict) (a : string) = exclave_ (Failure a)
[%%expect{|
val alloc_exception : string -> exn @ local = <fun>
|}]
let (alloc_exception @ noalloc) (a : string) = exclave_ (Failure a)
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
Error: The allocation is "local"
         because it is inside the function at lines 1-2, characters 40-20
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]
let (alloc_atomic_loc @ noalloc) (r : atomic_record) =
  [%atomic.loc r.af]
[%%expect{|
Line 2, characters 2-20:
2 |   [%atomic.loc r.af]
      ^^^^^^^^^^^^^^^^^^
Error: The allocation is "local"
         because it is inside the function at lines 1-2, characters 33-20
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

let (alloc_atomic_loc @ noalloc_strict) (r : atomic_record) = exclave_
  [%atomic.loc r.af]
[%%expect{|
val alloc_atomic_loc : atomic_record -> string atomic_loc @ local = <fun>
|}]
let (alloc_atomic_loc @ noalloc) (r : atomic_record) = exclave_
  [%atomic.loc r.af]
[%%expect{|
val alloc_atomic_loc : atomic_record -> string atomic_loc @ local = <fun>
|}]

(* Coercing a function with an optional argument to a plain arrow eta-expands
   it, allocating a closure. We receive both functions as parameters (in a
   tuple to avoid currying closures) so the only allocation is the coercion
   closure. *)
let (alloc_fun_coerce @ noalloc_strict)
      ((apply, opt) :
         (((unit -> int) -> int) * (?x:int -> unit -> int)) @ noalloc_strict) =
  let _ = apply opt in ()
[%%expect{|
Line 4, characters 16-19:
4 |   let _ = apply opt in ()
                    ^^^
Error: The allocation is "local"
         because it is inside the function at lines 2-4, characters 6-25
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]
let (alloc_fun_coerce @ noalloc)
      ((apply, opt) :
         (((unit -> int) -> int) * (?x:int -> unit -> int)) @ noalloc) =
  let _ = apply opt in ()
[%%expect{|
Line 4, characters 16-19:
4 |   let _ = apply opt in ()
                    ^^^
Error: The allocation is "local"
         because it is inside the function at lines 2-4, characters 6-25
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

(* A list/array comprehension allocates its result *)
let (alloc_list_comprehension @ noalloc_strict) () = let _ = [ x for x = 1 to 10 ] in ()
[%%expect{|
Line 1, characters 61-82:
1 | let (alloc_list_comprehension @ noalloc_strict) () = let _ = [ x for x = 1 to 10 ] in ()
                                                                 ^^^^^^^^^^^^^^^^^^^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 48-88
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]
let (alloc_list_comprehension @ noalloc) () = let _ = [ x for x = 1 to 10 ] in ()
[%%expect{|
Line 1, characters 54-75:
1 | let (alloc_list_comprehension @ noalloc) () = let _ = [ x for x = 1 to 10 ] in ()
                                                          ^^^^^^^^^^^^^^^^^^^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 41-81
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

let (stack_comprehension @ noalloc_strict) () = exclave_ [ x for x = 1 to 10 ]
[%%expect{|
Line 1, characters 57-78:
1 | let (stack_comprehension @ noalloc_strict) () = exclave_ [ x for x = 1 to 10 ]
                                                             ^^^^^^^^^^^^^^^^^^^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 43-78
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

(* Array indexing uses the [%array_safe_get] primitive. Whether it allocates
   depends on the element type -- [int array] does not box, [float array] does
   -- which is why the decision is taken at the application site, once the
   array type is known. *)
let array_get_prim_poly (a : int array) (i : int) =
  let (f @ noalloc_strict) () = a.(i) in
  f ()
[%%expect{|
val array_get_prim_poly : int array -> int -> int = <fun>
|}]

(* [Prim_poly] result ([@local_opt]): [%makemutable] allocates, at the
   result's locality. Inside a [noalloc] function that locality is forced to
   local (a stack allocation is allowed there), so the error we get is about
   the local ref escaping rather than about the allocation itself. *)
external mk_poly : 'a -> ('a ref[@local_opt]) = "%makemutable"
let (prim_poly_result @ noalloc_strict) (x : int) = mk_poly x
[%%expect{|
external mk_poly : 'a -> ('a ref [@local_opt]) = "%makemutable"
Line 2, characters 52-61:
2 | let (prim_poly_result @ noalloc_strict) (x : int) = mk_poly x
                                                        ^^^^^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

(* [Prim_global] result: [%makemutable] allocates on the heap. *)
external mk_global : 'a -> 'a ref = "%makemutable"
let (prim_global_result @ noalloc_strict) (x : int) = mk_global x
[%%expect{|
external mk_global : 'a -> 'a ref = "%makemutable"
Line 2, characters 54-63:
2 | let (prim_global_result @ noalloc_strict) (x : int) = mk_global x
                                                          ^^^^^^^^^
Error: The allocation is "local"
         because it is allocated inside the function at line 2, characters 42-65,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

(* [Prim_global] result with a [Prim_poly] argument: [%makemutable] allocates
   on the heap. *)
external mk_polyarg : ('a[@local_opt]) -> 'a ref = "%makemutable"
let (prim_global_result_poly_arg @ noalloc_strict) (x : int) = mk_polyarg x
[%%expect{|
external mk_polyarg : ('a [@local_opt]) -> 'a ref = "%makemutable"
Line 2, characters 63-73:
2 | let (prim_global_result_poly_arg @ noalloc_strict) (x : int) = mk_polyarg x
                                                                   ^^^^^^^^^^
Error: The allocation is "local"
         because it is allocated inside the function at line 2, characters 51-75,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

(* [Prim_local] result: [%makemutable] compiles to [Pmakeblock] at the result's
   locality, so a local result really is a stack allocation, which does not
   count on the allocation axis. *)
external mk_local : 'a -> local_ 'a ref = "%makemutable"
let (prim_local_result @ noalloc_strict) (x : int) = exclave_ mk_local x
[%%expect{|
external mk_local : 'a -> 'a ref @ local = "%makemutable"
val prim_local_result : int -> int ref @ local = <fun>
|}]

(* [my_id] (a fully-applied [%identity]) does not force the enclosing function
   to [alloc], so this is accepted. *)
external my_id : ('a[@local_opt]) -> ('a[@local_opt]) = "%identity"
let (prim_value_captured @ noalloc_strict) (x : int) = my_id x
[%%expect{|
external my_id : ('a [@local_opt]) -> ('a [@local_opt]) = "%identity"
val prim_value_captured : int -> int = <fun>
|}]

(* [%opaque] compiles to [Popaque], which does not allocate. *)
external opaque_prim : ('a[@local_opt]) -> ('a[@local_opt]) = "%opaque"
let (f @ noalloc_strict) (x : int) = opaque_prim x
[%%expect{|
external opaque_prim : ('a [@local_opt]) -> ('a [@local_opt]) = "%opaque"
val f : int -> int = <fun>
|}]

(* [!] compiles to [Pfield], which does not allocate. *)
let (deref_value_captured @ noalloc_strict) (r : int ref) = !r
[%%expect{|
val deref_value_captured : int ref -> int = <fun>
|}]

(* The allocation behaviour of these primitives depends on the types at the
   occurrence, so it can only be decided once the arguments have been typed.
   Cross-check any change here against [ocamlc -dlambda] on the same source.
   These are written with a single parameter so that no intermediate closure
   shows up in the printed type. *)

(* [%compare] at [string] compiles to [caml_string_compare], declared
   [[@@noalloc]]; at [int] it compiles to a [Pscalar] comparison. *)
let (cmp_string @ noalloc_strict) (x : string) = compare x "a"
let (cmp_int @ noalloc_strict) (x : int) = compare x 0
[%%expect{|
val cmp_string : string -> int = <fun>
val cmp_int : int -> int = <fun>
|}]

(* At an unknown type it compiles to [caml_compare], which does allocate. *)
let (cmp_poly @ noalloc_strict) x = compare x x
[%%expect{|
Line 1, characters 36-43:
1 | let (cmp_poly @ noalloc_strict) x = compare x x
                                        ^^^^^^^
Error: The allocation is "local"
         because it is allocated inside the function at line 1, characters 32-47,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

(* CR shsong: Why this is a integer rather than integeter option comparison? *)
(* A constant constructor as an argument specializes the generic equality to an
   integer comparison, even though the type alone would not allow it. *)
let (eq_const @ noalloc_strict) (x : int option) = x = None
[%%expect{|
val eq_const : int option -> bool = <fun>
|}]

(* Reading an [int array] does not box; reading a [float array] does. *)
let (get_int @ noalloc_strict) (a : int array) = Array.unsafe_get a 0
[%%expect{|
val get_int : int array -> int = <fun>
|}]
let (get_float @ noalloc_strict) (a : float array) = Array.unsafe_get a 0
[%%expect{|
Line 1, characters 53-69:
1 | let (get_float @ noalloc_strict) (a : float array) = Array.unsafe_get a 0
                                                         ^^^^^^^^^^^^^^^^
Error: The allocation is "local"
         because it is allocated inside the function at line 1, characters 33-73,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

(* A C external declared [[@@noalloc]] does not allocate; the same symbol
   declared without the attribute is assumed to. *)
external eq_nofail : string -> string -> bool = "caml_string_equal" [@@noalloc]
let (c_noalloc @ noalloc_strict) (x : string) = eq_nofail x "a"
[%%expect{|
external eq_nofail : string -> string -> bool = "caml_string_equal"
  [@@noalloc]
val c_noalloc : string -> bool = <fun>
|}]
external eq_may_alloc : string -> string -> bool = "caml_string_equal"
let (c_alloc @ noalloc_strict) (x : string) = eq_may_alloc x "a"
[%%expect{|
external eq_may_alloc : string -> string -> bool = "caml_string_equal"
Line 2, characters 46-58:
2 | let (c_alloc @ noalloc_strict) (x : string) = eq_may_alloc x "a"
                                                  ^^^^^^^^^^^^
Error: The allocation is "local"
         because it is allocated inside the function at line 2, characters 31-64,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

(* A [Prim_poly] allocating primitive is accepted when its result does not
   escape: the allocation can go on the stack, which a [noalloc] function
   allows. Contrast with [prim_poly_result] above, where the ref escapes. *)
let (poly_alloc_not_escaping @ noalloc_strict) (x : int) =
  let r = mk_poly x in
  !r
[%%expect{|
val poly_alloc_not_escaping : int -> int = <fun>
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
Error: The allocation is "local"
         because it is inside the function at line 1, characters 34-62
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
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
Error: The allocation is "local"
         because it is inside the function at lines 12-17, characters 34-3
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global"
         because it is an allocation
         which is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
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
Error: The allocation is "local"
         because it is inside the function at lines 1-6, characters 26-3
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global"
         because it is an allocation
         which is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
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
Error: The allocation is "local"
         because it is inside the function at lines 2-4, characters 27-5
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global"
         because it is an allocation
         which is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
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
Error: The allocation is "local"
         because it is inside the function at line 3, characters 29-40
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
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
Error: The allocation is "local"
         because it is inside the function at line 1, characters 34-48
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global"
         because it is an allocation
         which is expected to be "local" to the parent region or "global"
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
Error: The allocation is "local"
         because it is inside the function at line 1, characters 27-41
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global"
         because it is an allocation
         which is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

let (f_explicit_exclave @ noalloc) x = exclave_ fun y -> x
[%%expect{|
val f_explicit_exclave : 'a -> ('b -> 'a) @ local = <fun>
|}]

(* Other variations to test whether we can detect different format of noalloc
  annotation. *)
let f_explicit: _ @ noalloc_strict = fun x -> (fun y -> x)
[%%expect{|
Line 1, characters 46-58:
1 | let f_explicit: _ @ noalloc_strict = fun x -> (fun y -> x)
                                                  ^^^^^^^^^^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 37-58
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global"
         because it is an allocation
         which is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

let f_explicit_exclave: _ @ noalloc_strict = fun x -> exclave_ (fun y -> x)
[%%expect{|
val f_explicit_exclave : 'a -> ('b -> 'a) @ local = <fun>
|}]

let f_explicit = (fun x -> (fun y -> x) : (int -> int -> int) @ noalloc_strict)
[%%expect{|
Line 1, characters 27-39:
1 | let f_explicit = (fun x -> (fun y -> x) : (int -> int -> int) @ noalloc_strict)
                               ^^^^^^^^^^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 18-39
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

let f_explicit_exclave = (fun x -> exclave_ (fun y -> x) : (int -> (int -> int) @ local) @ noalloc_strict)
[%%expect{|
val f_explicit_exclave : int -> (int -> int) @ local = <fun>
|}]

let f_explicit = (fun x -> (fun y -> x) : _ @ noalloc_strict)
[%%expect{|
Line 1, characters 27-39:
1 | let f_explicit = (fun x -> (fun y -> x) : _ @ noalloc_strict)
                               ^^^^^^^^^^^^
Error: The allocation is "local"
         because it is inside the function at line 1, characters 18-39
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global"
         because it is an allocation
         which is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

let f_explicit_exclave = (fun x -> exclave_ (fun y -> x) : _ @ noalloc_strict)
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
Error: The allocation is "local"
         because it is inside the function at line 1, characters 36-48
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
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
Error: The allocation is "local"
         because it is inside the function at line 8, characters 58-70
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
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

(** Test 6.3: A closure built inside a [noalloc_strict] / [noalloc] function
    (if the information is known when type check the function) is forced to
    be [local] and on stack. It is also exempted from the allocation check.
    We probe each syntactic position for a nested closure.
*)

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

(* Closure created inside mode annotation on the returned closure
   ([mode_morph]/[type_expect_mode]). We annotate a NON-locality axis
   ([contended]) on purpose: the [contended] in the result shows the
   annotation itself took effect (so the [mode_morph] path is exercised),
   while the [@ local] codomain is enforced by annotating the outer
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

(* Record field via a modality ([mode_is_contained_by]). *)
type box = { g : unit -> int }
[%%expect{|
type box = { g : unit -> int; }
|}]
let (p_record @ noalloc_strict) (x : int) = exclave_ { g = (fun () -> x) }
[%%expect{|
val p_record : int -> box @ local = <fun>
|}]

(* [let]-binding RHS. *)
let (g_let @ noalloc_strict) (x : int) =
  let h = fun () -> x in
  h ()
[%%expect{|
Line 2, characters 10-21:
2 |   let h = fun () -> x in
              ^^^^^^^^^^^
Error: The allocation is "local"
         because it is inside the function at lines 1-3, characters 29-6
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global"
         because it is an allocation
         which is expected to be "local" to the parent region or "global"
         because it is the function in a tail call.
|}]

(* Application argument. *)
let g_app_arg () =
  let (use @ noalloc_strict) (g : unit -> int) = g () in
  let (g_arg @ noalloc_strict) (x : int) = use (fun () -> x) in
  ()
[%%expect{|
Line 3, characters 47-60:
3 |   let (g_arg @ noalloc_strict) (x : int) = use (fun () -> x) in
                                                   ^^^^^^^^^^^^^
Error: The allocation is "local"
         because it is inside the function at line 3, characters 31-60
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global"
         because it is an allocation
         which is expected to be "local" to the parent region or "global"
         because it is an argument in a tail call.
|}]

(* Region body ([for]/[while]/comprehension). *)
let (g_region @ noalloc_strict) (x : int) =
  for _i = 0 to 0 do
    let _g = fun () -> x in ()
  done
[%%expect{|
val g_region : int -> unit = <fun>
|}]

(* Special case: Let-binding operator (letop): the [let*] body is a continuation
   closure typed at legacy via [mode_return] in [type_binding_op]
   (the [mode_return Value.legacy] there), so we cannot make it local.
   In practice the failure surfaces even earlier: the [let*] operator is itself
   an [alloc] value, so referencing it inside a [noalloc_strict] function trips
   the capture rule first. Either way, [let*] does not currently compose with
   [noalloc_strict]. *)
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

(** Test 6.4: Misc test regarding abstract type

    CR shsong: Chek whether backend guarantee that foo does not allocate for the
    returned global closure g0. *)
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

(** Test 6.4: Nested noalloc functions *)
(* CR shsong: f cannot be implied as noalloc_strict unless explicitly annotated.
    Analysis: In walk_locks_for_allocation, we force closures to be alloc until
    we encounter a [Closure_noalloc_lock]. Everything outside [Closure_noalloc_lock]
    will still be forced to be alloc.
    An alternative implementation: loop over all locks and see whether there is a
    [Closure_noalloc_lock] before adding any submode constraints.
    Tradeoff:
    1. the alternative implementation should allow us to accept the
    [nested_noalloc_func_implicit] case. However, the current implementation is ok
    if we expect user to always annotate functions if they want it them to be noalloc.
    2. the alternative implementation avoids the current check on whether the
    Allocation mode axis is changed while walking locks.
    3. the alternative implementation requires iterate over the locks twice, which
    affects the performance.
*)
let nested_noalloc_func_implicit () =
  let f () = exclave_
    let (g @ noalloc_strict) () = exclave_ (fun x -> x) in
    g
  in
  (f : _ @ noalloc_strict)
[%%expect{|
val nested_noalloc_func_implicit : unit -> unit -> (unit -> 'a -> 'a) @ local =
  <fun>
|}]

let nested_noalloc_func_explicit () =
  let (f @ noalloc_strict) () = exclave_
    let (g @ noalloc_strict) () = exclave_ (fun x -> x) in
    g
  in
  (f : _ @ noalloc_strict)
[%%expect{|
val nested_noalloc_func_explicit : unit -> unit -> (unit -> 'a -> 'a) @ local =
  <fun>
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
Error: The allocation is "local"
         because it is inside the function at line 2, characters 29-54
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
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
Error: The allocation is "local"
         because it is inside the function at lines 1-3, characters 35-16
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global"
         because it is an allocation
         which is expected to be "local" to the parent region or "global"
         because it is the function in a tail call.
|}]

let (secretly_allocates' @ noalloc) () = exclave_
 let mutable escape_hatch = None in
 (escape_hatch <- stack_ Some (fun () -> { x = 1.; y = 2. })) [@zero_alloc];
 match escape_hatch with
 | None -> stack_ { x = 1.; y = 2. }
 | Some f -> f ()
[%%expect{|
Line 3, characters 41-59:
3 |  (escape_hatch <- stack_ Some (fun () -> { x = 1.; y = 2. })) [@zero_alloc];
                                             ^^^^^^^^^^^^^^^^^^
Error: The allocation is "local"
         because it is inside the function at lines 1-6, characters 36-17
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]

let (secretly_allocates @ noalloc) () = exclave_
 let mutable (escape_hatch @ alloc) = None in
 (escape_hatch <- stack_ Some (fun () -> { x = 1.; y = 2. })) [@zero_alloc];
 match escape_hatch with
 | None -> stack_ { x = 1.; y = 2. }
 | Some f -> f ()
[%%expect{|
Line 3, characters 41-59:
3 |  (escape_hatch <- stack_ Some (fun () -> { x = 1.; y = 2. })) [@zero_alloc];
                                             ^^^^^^^^^^^^^^^^^^
Error: The allocation is "local"
         because it is inside the function at lines 1-6, characters 35-17
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
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
Error: The allocation is "local"
         because it is inside the function at line 1, characters 32-50
         which is required not to allocate.
       However, the allocation highlighted is expected to be "global".
|}]


(** Test 8: [module type of] and the allocation axis

  [module type of M] must produce a self-contained module type on the spot, so
  it zaps the modalities of [M]'s items while type-checking -- long before
  [optimise_allocations] settles the allocation axis. [constrain_closures] is
  therefore called immediately before that zap: every allocation already known
  to be on the heap has by then forced its enclosing closures to [alloc], so
  the floor the zap reads is correct.

  What makes that possible is [check_nongen_modtype], which runs just before
  and defaults the arrow modes reachable from the scraped signature; the
  areality of the allocations surfacing there is therefore known. *)

(* Case 1: the functions are annotated. The annotation survives. *)
module M_annot = struct
  let (f @ noalloc_strict) (x : int) = x + 1
  let (g @ noalloc) (x : int) = x + 1
end
module type S_annot = module type of M_annot
[%%expect{|
module M_annot :
  sig val f : int -> int @@ noalloc_strict val g : int -> int end @@ portable
  noalloc
module type S_annot =
  sig
    val f : int -> int @@ portable noalloc_strict
    val g : int -> int @@ portable noalloc
  end
|}]

(* Case 2: the functions are not annotated but do not allocate;
   [noalloc_strict] is still inferred. *)
module M_infer = struct
  let f (x : int) = x + 1
end
module type S_infer = module type of M_infer
[%%expect{|
module M_infer : sig val f : int -> int end @@ portable noalloc_strict
module type S_infer = sig val f : int -> int @@ portable noalloc_strict end
|}]

(* Projecting out of the module still works afterwards: the zap reads a floor
   that is correct, rather than pinning the value to [alloc]. *)
module M_proj = struct
  let f (x : int) = x + 1
end
module type S_proj = module type of M_proj
let () = ignore (M_proj.f : _ @ noalloc_strict)
[%%expect{|
module M_proj : sig val f : int -> int end @@ portable noalloc_strict
module type S_proj = sig val f : int -> int @@ portable noalloc_strict end
|}]

(* Case 3: the function really does allocate on the heap. [constrain_closures]
   has already forced it to [alloc], so the module type reports [alloc]: no
   [noalloc_strict] is claimed, and no error is raised. *)
module M_alloc = struct
  let pack x = [| x |]
end
module type S_alloc = module type of M_alloc
[%%expect{|
module M_alloc : sig val pack : ('a : value_maybe_null). 'a -> 'a array end
  @@ stateless
module type S_alloc =
  sig val pack : ('a : value_maybe_null). 'a -> 'a array @@ stateless end
|}]

(* Case 4: the allocation is internal to a function body, so its areality need
   not be reachable from the scraped signature. That is harmless: an allocation
   unreachable from the signature is also unreachable by any later constraint,
   so [optimise_allocations] pushes it to [local] and a [noalloc_strict] claim
   would be correct. *)
module M_internal = struct
  let f () = let g () = (1, 2) in let (a, b) = g () in a + b
end
module type S_internal = module type of M_internal
[%%expect{|
module M_internal : sig val f : unit -> int end @@ portable
module type S_internal = sig val f : unit -> int @@ portable end
|}]

(* Case 5: the allocation lives in a nested module and is only forced onto the
   heap after the [module type of]. *)
module M_nested = struct
  module Inner = struct let g () = (1, 2) end
end
module type S_nested = module type of M_nested
let sink : (int * int) ref = ref (0, 0)
let () = sink := M_nested.Inner.g ()
[%%expect{|
module M_nested : sig module Inner : sig val g : unit -> int * int end end @@
  stateless
module type S_nested =
  sig module Inner : sig val g : unit -> int * int @@ stateless end end
val sink : (int * int) ref @@ stateless noalloc_strict = {contents = (0, 0)}
|}]

(* Case 6: the same, through a functor inside the module. *)
module M_functor = struct
  module F (X : sig end) = struct let g () = (1, 2) end
end
module type S_functor = module type of M_functor
[%%expect{|
module M_functor :
  sig
    module F :
      functor (X : sig end) -> sig val g : unit -> int * int @@ stateless end
  end @@ stateless
module type S_functor =
  sig
    module F :
      functor (X : sig end) -> sig val g : unit -> int * int @@ stateless end
      @@ stateless
  end
|}]

(* [include functor] in a signature is the other zap that runs while
   type-checking. It takes a functor *module type*, whose modalities are
   written by the user and so already constant, leaving the zap nothing to do
   -- including when that module type itself came from [module type of]. *)
module Fm (X : sig end) = struct let g () = (1, 2) end
module type FT = module type of Fm
module type S_incl = sig include functor FT end
[%%expect{|
module Fm :
  functor (X : sig end) -> sig val g : unit -> int * int @@ stateless end @@
  stateless
module type FT =
  functor (X : sig end) -> sig val g : unit -> int * int @@ stateless end
module type S_incl = sig val g : unit -> int * int @@ stateless end
|}]
