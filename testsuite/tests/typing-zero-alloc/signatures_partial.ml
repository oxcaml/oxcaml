(* TEST
   expect.opt;
*)

module type Exact = sig
  val add : int -> int -> int [@@zero_alloc]
end
module type Partial = sig
  val add : int -> int -> int [@@zero_alloc partial]
end
module Exact : Exact = struct let add x y = x + y end
module Partial : Partial = struct let add x y = x + y end
[%%expect{|
module type Exact = sig val add : int -> int -> int [@@zero_alloc] end
module type Partial =
  sig val add : int -> int -> int [@@zero_alloc partial] end
module Exact : Exact
Line 8, characters 42-53:
8 | module Partial : Partial = struct let add x y = x + y end
                                              ^^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP4.Partial.add (camlTOP4__add_1_3_code).
       Partial applications of this function may allocate a closure on the heap.
       Hint: try marking the partial function "local", as in "'a -> ('b -> ... -> 'z) @ local".
|}]

(* With [@ local], both modules above are fine: *)
module type Exact = sig
  val add : int -> (int -> int) @ local [@@zero_alloc]
end
module type Partial = sig
  val add : int -> (int -> int) @ local [@@zero_alloc partial]
end
module Exact : Exact = struct let add x y = x + y end
module Partial : Partial = struct let add x y = x + y end
[%%expect{|
module type Exact =
  sig val add : int -> (int -> int) @ local [@@zero_alloc] end
module type Partial =
  sig val add : int -> (int -> int) @ local [@@zero_alloc partial] end
module Exact : Exact
module Partial : Partial
|}]

(* We can *use* [Partial] where we couldn't use [Exact]: *)
let[@zero_alloc] add_42_partial () = exclave_ Partial.add 42
let[@zero_alloc] add_42_exact () = exclave_ Exact.add 42
[%%expect{|
val add_42_partial : unit -> (int -> int) @ local [@@zero_alloc arity 1] =
  <fun>
Line 2, characters 5-15:
2 | let[@zero_alloc] add_42_exact () = exclave_ Exact.add 42
         ^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP10.add_42_exact (camlTOP10__add_42_exact_5_11_code).
Line 2, characters 44-56:
2 | let[@zero_alloc] add_42_exact () = exclave_ Exact.add 42
                                                ^^^^^^^^^^^^
Error: called function may allocate (indirect tailcall)
|}]

(* "Partial" does not vacuously apply to the zero-argument case: *)
module Not_a_function_partial : sig
  val i : int [@@zero_alloc partial]
end = struct
  let i = 42
end
[%%expect{|
Line 2, characters 2-36:
2 |   val i : int [@@zero_alloc partial]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In signatures, zero_alloc is only supported on function declarations.
       Found no arrows in this declaration's type.
       Hint: You can write "[@zero_alloc arity n]" to specify the arity
       of an alias (for n > 0).
|}]

(* To be used in the test below: *)
module Identity_partial : sig
  val id : 'a -> 'a [@@zero_alloc partial]
end = struct
  let id x = x
end
[%%expect{|
module Identity_partial : sig val id : 'a -> 'a [@@zero_alloc partial] end
|}]

(* Over-application does not count as "partial": *)
module Identity_overapplied_partial : sig
  val f : int -> (int -> int) @ local [@@zero_alloc partial]
end = struct
  let f x y = Identity_partial.id (fun x y -> x + y) x y
end
[%%expect{|
Line 4, characters 8-56:
4 |   let f x y = Identity_partial.id (fun x y -> x + y) x y
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP13.Identity_overapplied_partial.f (camlTOP13__f_7_16_code).
Line 4, characters 14-56:
4 |   let f x y = Identity_partial.id (fun x y -> x + y) x y
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: called function may allocate (direct tailcall caml_apply3)
|}]

module Local_but_allocates_on_the_heap : sig
  val stack_allowed : int -> (int -> int) @ local [@@zero_alloc partial]
  val heap_required : int -> int (* implicitly [@ global] *)
end = struct
  let stack_allowed x y = x + y
  let heap_required = stack_allowed 42
end
[%%expect{|
Line 5, characters 20-31:
5 |   let stack_allowed x y = x + y
                        ^^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP14.Local_but_allocates_on_the_heap.stack_allowed (camlTOP14__stack_allowed_9_20_code).
       Partial applications of this function may allocate a closure on the heap.
       Hint: try marking the partial function "local", as in "'a -> ('b -> ... -> 'z) @ local".
|}]
