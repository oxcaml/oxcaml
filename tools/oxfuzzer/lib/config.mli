val max_function_count : int

val fun_min_param_count : int

val fun_max_param_count : int

val max_block_depth : int

val max_loop_stride : int

val max_loop_offset : int

val toplevel_var_count : int

val max_array_dimensions : int

val max_array_axis_size : int

val max_array_elements : int

val max_record_types : int

val max_record_fields : int

val max_exception_count : int

val max_handler_count : int

val max_try_count : int

val max_variant_types : int

val max_variant_constructors : int

val max_constructor_args : int

val max_match_count : int

val max_unboxed_tuple_arity : int

val max_unboxed_tuple_depth : int

(* Percentages. *)
val record_probability : int

val variant_probability : int

val or_null_probability : int

val unboxed_tuple_probability : int

val match_in_match_probability : int

val nested_record_probability : int

val mutable_binding_probability : int

val mutable_field_probability : int

val array_probability : int

val array_literal_probability : int

val bounded_index_probability : int

val opaque_initializer_probability : int

val opaque_leaf_probability : int

val opaque_loop_bound_probability : int

val exception_payload_probability : int

val payload_binding_probability : int

val catch_all_probability : int

val raise_notrace_probability : int

val callee_raises_probability : int

type assertion_mode =
  | Disabled
  | Unreachable_only
  | May_raise
      (** Assertion failures are observed with initialized state, rather than
          treated as errors. *)

module Swarm : sig
  type t =
    { floats : bool;
      unboxed_numbers : bool;
      arrays : bool;
      multidimensional_arrays : bool;
      boxed_records : bool;
      unboxed_records : bool;
      unboxed_tuples : bool;
      variants : bool;
      or_null : bool;
      matches : bool;
      assertions : assertion_mode;
      record_updates : bool;
      record_representation_conversions : bool;
      mutable_bindings : bool;
      mutable_record_fields : bool;
      array_writes : bool;
      function_calls : bool;
      always_inline : bool;
      never_inline : bool;
      conditionals : bool;
      bounded_loops : bool;
      bitwise_operations : bool;
      opaque_initializers : bool;
      opaque_leaves : bool;
      opaque_loop_bounds : bool;
      exceptions : bool
    }

  val create : Random.State.t -> t

  val to_string : t -> string

  val number_types : t -> Ir.NumberTy.t list
end
