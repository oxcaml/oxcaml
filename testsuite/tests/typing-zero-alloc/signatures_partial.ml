(* TEST
   expect.opt;
*)

module type Exact = sig val add : int -> int -> int [@@zero_alloc] end
module type Partial = sig val add : int -> int -> int [@@zero_alloc partial] end
module Exact : Exact = struct let add x y = x + y end
module Partial : Partial = struct let add x y = x + y end
[%%expect{|
module type Exact = sig val add : int -> int -> int [@@zero_alloc] end
module type Partial =
  sig val add : int -> int -> int [@@zero_alloc partial] end
module Exact : Exact
Line 4, characters 42-53:
4 | module Partial : Partial = struct let add x y = x + y end
                                              ^^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP4.Partial.add (camlTOP4__add_1_3_code).
       Partial applications of this function may allocate a closure on the heap.
       Hint: try marking the partial function "local", as in "'a -> ('b -> ... -> 'z) @ local".
|}]

module type PartialWithLocal = sig
  val add : int -> (int -> int) @ local [@@zero_alloc partial]
end
module PartialWithLocal : PartialWithLocal = struct let add x y = x + y end
[%%expect{|
module type PartialWithLocal =
  sig val add : int -> (int -> int) @ local [@@zero_alloc partial] end
module PartialWithLocal : PartialWithLocal
|}]

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

module Identity_partial : sig
  val id : 'a -> 'a [@@zero_alloc partial]
end = struct
  let id x = x
end
[%%expect{|
module Identity_partial : sig val id : 'a -> 'a [@@zero_alloc partial] end
|}]

module Identity_overapplied_partial : sig
  val f : int -> int -> int [@@zero_alloc partial]
end = struct
  let f x y = Identity_partial.id (fun x y -> x + y) x y
end
[%%expect{|
Line 4, characters 8-56:
4 |   let f x y = Identity_partial.id (fun x y -> x + y) x y
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP9.Identity_overapplied_partial.f (camlTOP9__f_4_10_code).
       Partial applications of this function may allocate a closure on the heap.
       Hint: try marking the partial function "local", as in "'a -> ('b -> ... -> 'z) @ local".
|}]
