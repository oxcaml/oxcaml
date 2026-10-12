(* TEST
   flags += " -extension mode_alpha";
   expect.opt;
*)

exception Allocations_don't_count of float
let r = ref 42.
[%%expect{|
exception Allocations_don't_count of float
val r : float ref = {contents = 42.}
|}]

module Allocations_before_exceptions_don't_count : sig
  val f : exceptional:bool -> unit @@ noalloc
end = struct
  let f ~exceptional = if exceptional then raise (Allocations_don't_count !r)
end
[%%expect{|
CR wsturgeon for wsturgeon: this has to pass
|}]

module Unless_strict : sig
  val f : exceptional:bool -> unit @@ noalloc_strict
end = struct
  let f ~exceptional = if exceptional then raise (Allocations_don't_count !r)
end
[%%expect{|
Line 4, characters 49-77:
4 |   let f ~exceptional = if exceptional then raise (Allocations_don't_count !r)
                                                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The allocation is "local"
         because it is allocated inside the function at line 4, characters 8-77,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]
