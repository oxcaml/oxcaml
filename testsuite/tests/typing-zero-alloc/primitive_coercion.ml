(* TEST
   (* Compile [stubs.cmxs] to use with [expect.opt] *)
   readonly_files = "stubs.c";
   setup-ocamlopt.opt-build-env;
   program = "stubs.cmxs";
   flags = "-shared";
   all_modules = "stubs.c";
   ocamlopt.opt;
   src = "stubs.cmxs";
   dst = "${test_build_directory_prefix}/";
   copy;

   flags = "stubs.cmxs";
   expect.opt;
*)

module Nonallocating_primitive : sig
  val add : int8# -> int8# -> int8# [@@zero_alloc]
end = struct
  external add : int8# -> int8# -> int8# = "%int8#_add"
end
[%%expect {|
module Nonallocating_primitive :
  sig val add : int8# -> int8# -> int8# [@@zero_alloc] end
|}]

module Allocating_primitive : sig
  val add : int32 -> int32 -> int32 [@@zero_alloc]
end = struct
  external add : int32 -> int32 -> int32 = "%int32_add"
end
[%%expect {|
Line 2, characters 39-49:
2 |   val add : int32 -> int32 -> int32 [@@zero_alloc]
                                           ^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP2.Allocating_primitive.(partial) (camlTOP2__fn[:4,2--55]_2_1_code).
Line 4, characters 2-55:
4 |   external add : int32 -> int32 -> int32 = "%int32_add"
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: allocation of 24 bytes for boxed_int32
|}]

module Allocating_c_stub : sig
  val add : float -> float -> float [@@zero_alloc]
end = struct
  external add : float -> float -> float = "caml_add_float"
end
[%%expect {|
Line 2, characters 39-49:
2 |   val add : float -> float -> float [@@zero_alloc]
                                           ^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP3.Allocating_c_stub.(partial) (camlTOP3__fn[:4,2--59]_4_2_code).
Line 4, characters 2-59:
4 |   external add : float -> float -> float = "caml_add_float"
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: called function may allocate (external call to caml_add_float)
|}]

module Non_allocating_c_stubs : sig
  val to_int : float -> int [@@zero_alloc]
  val add : float -> float -> float [@@zero_alloc]
end = struct
  external to_int : float -> int = "caml_int_of_float" [@@noalloc]
  external add : float -> float -> float = "caml_add_float" [@@noalloc]
  (* Notice [caml_add_float] does allocate. [[@@noalloc]] does not actually
     check *)
end
[%%expect {|
module Non_allocating_c_stubs :
  sig
    val to_int : float -> int [@@zero_alloc]
    val add : float -> float -> float [@@zero_alloc]
  end
|}]

module Non_allocating_builtin : sig
  val select : bool -> int32 -> int32 -> int32 [@@zero_alloc]
end = struct
  external select : bool -> int32 -> int32 -> int32 = "caml_csel_value" [@@builtin]
end
[%%expect {|
module Non_allocating_builtin :
  sig val select : bool -> int32 -> int32 -> int32 [@@zero_alloc] end
|}]

module Allocating_builtin : sig
  val float_to_int64 : float32 -> int64 [@@zero_alloc]
end = struct
  external float_to_int64 : float32 -> int64 =
    "" "caml_float32_to_int64" [@@builtin] [@@unboxed] [@@noalloc]
end
[%%expect {|
Line 2, characters 43-53:
2 |   val float_to_int64 : float32 -> int64 [@@zero_alloc]
                                               ^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP6.Allocating_builtin.(partial) (camlTOP6__fn[:4,2--113]_12_6_code).
Lines 4-5, characters 2-66:
4 | ..external float_to_int64 : float32 -> int64 =
5 |     "" "caml_float32_to_int64" [@@builtin] [@@unboxed] [@@noalloc]
Error: allocation of 24 bytes for boxed_int64
|}]

module Primitive_could_be_made_non_allocating_with_constraint : sig
  val equal : 'a -> 'a -> bool [@@zero_alloc]
end = struct
  external equal : 'a -> 'a -> bool = "%equal"
end
[%%expect {|
Line 2, characters 34-44:
2 |   val equal : 'a -> 'a -> bool [@@zero_alloc]
                                      ^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP7.Primitive_could_be_made_non_allocating_with_constraint.(partial) (camlTOP7__fn[:4,2--46]_14_7_code).
Line 4, characters 2-46:
4 |   external equal : 'a -> 'a -> bool = "%equal"
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: called function may allocate (external call to caml_equal)
|}]

module Primitive_made_non_allocating_with_constraint : sig
  val equal : ('a : immediate). 'a -> 'a -> bool [@@zero_alloc]
end = struct
  external equal : 'a -> 'a -> bool = "%equal"
  (* Polymorphic equality allocates, but only if ['a] is a pointer *)
end
[%%expect {|
module Primitive_made_non_allocating_with_constraint :
  sig val equal : ('a : immediate). 'a -> 'a -> bool [@@zero_alloc] end
|}]

module Overapplied_primitive : sig
  val select_and_apply : ('a -> 'b) array -> int -> 'a -> 'b [@@zero_alloc]
end = struct
  external select_and_apply : 'a array -> int -> 'a = "%array_safe_get"
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   external select_and_apply : 'a array -> int -> 'a = "%array_safe_get"
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           external select_and_apply : 'a array -> int -> 'a
             = "%array_safe_get"
         end
       is not included in
         sig
           val select_and_apply : ('a -> 'b) array -> int -> 'a -> 'b
             [@@zero_alloc]
         end
       Values do not match:
         external select_and_apply : 'a array -> int -> 'a
           = "%array_safe_get"
       is not included in
         val select_and_apply : ('a -> 'b) array -> int -> 'a -> 'b
           [@@zero_alloc]
       zero_alloc arity mismatch:
       When using "zero_alloc" in a signature, the syntactic arity of
       the implementation must match the function type in the interface.
       Here the former is 2 and the latter is 3.
|}]

module Underapplied_primitive : sig
  type 'a t = int -> 'a
  val make : 'a array -> 'a t [@@zero_alloc]
end = struct
  type 'a t = int -> 'a
  external make : 'a array -> int -> 'a = "%array_safe_get"
end
[%%expect {|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type 'a t = int -> 'a
6 |   external make : 'a array -> int -> 'a = "%array_safe_get"
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type 'a t = int -> 'a
           external make : 'a array -> int -> 'a = "%array_safe_get"
         end
       is not included in
         sig
           type 'a t = int -> 'a
           val make : 'a array -> 'a t [@@zero_alloc]
         end
       Values do not match:
         external make : 'a array -> int -> 'a = "%array_safe_get"
       is not included in
         val make : 'a array -> 'a t [@@zero_alloc]
       zero_alloc arity mismatch:
       When using "zero_alloc" in a signature, the syntactic arity of
       the implementation must match the function type in the interface.
       Here the former is 2 and the latter is 1.
|}]

module Allocating_primitive_not_checked_with_zero_alloc_opt : sig
  val add : int32 -> int32 -> int32 [@@zero_alloc opt]
end = struct
  external add : int32 -> int32 -> int32 = "%int32_add"
end
[%%expect {|
module Allocating_primitive_not_checked_with_zero_alloc_opt :
  sig val add : int32 -> int32 -> int32 [@@zero_alloc opt] end
|}]
