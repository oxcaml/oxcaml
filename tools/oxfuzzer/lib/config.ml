(* CR-someday hwasilewski: Make sure all constants, including probabilities, are
   inside Config. *)
let max_function_count = 10

let fun_min_param_count = 0

let fun_max_param_count = 5

let max_block_depth = 4

let max_loop_stride = 8

let max_loop_offset = 16

let toplevel_var_count = 5

let max_array_dimensions = 3

let max_array_axis_size = 10

let max_array_elements = 256

let max_record_types = 3

let max_record_fields = 4

(* Percentages. *)
let record_probability = 25

let nested_record_probability = 15

let mutable_binding_probability = 75

let mutable_field_probability = 50

let array_probability = 25

let array_literal_probability = 50

let bounded_index_probability = 75

let opaque_initializer_probability = 50

let opaque_leaf_probability = 5

let opaque_loop_bound_probability = 50

module Swarm = struct
  type t =
    { floats : bool;
      unboxed_numbers : bool;
      arrays : bool;
      multidimensional_arrays : bool;
      boxed_records : bool;
      unboxed_records : bool;
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
      opaque_loop_bounds : bool
    }

  let create random_state =
    { floats = Random.State.bool random_state;
      unboxed_numbers = Random.State.bool random_state;
      arrays = Random.State.bool random_state;
      multidimensional_arrays = Random.State.bool random_state;
      boxed_records = Random.State.bool random_state;
      unboxed_records = Random.State.bool random_state;
      record_updates = Random.State.bool random_state;
      record_representation_conversions = Random.State.bool random_state;
      mutable_bindings = Random.State.bool random_state;
      mutable_record_fields = Random.State.bool random_state;
      array_writes = Random.State.bool random_state;
      function_calls = Random.State.bool random_state;
      always_inline = Random.State.bool random_state;
      never_inline = Random.State.bool random_state;
      conditionals = Random.State.bool random_state;
      bounded_loops = Random.State.bool random_state;
      bitwise_operations = Random.State.bool random_state;
      opaque_initializers = Random.State.bool random_state;
      opaque_leaves = Random.State.bool random_state;
      opaque_loop_bounds = Random.State.bool random_state
    }

  let to_string t =
    [ "floats", t.floats;
      "unboxed_numbers", t.unboxed_numbers;
      "arrays", t.arrays;
      "multidimensional_arrays", t.multidimensional_arrays;
      "boxed_records", t.boxed_records;
      "unboxed_records", t.unboxed_records;
      "record_updates", t.record_updates;
      "record_representation_conversions", t.record_representation_conversions;
      "mutable_bindings", t.mutable_bindings;
      "mutable_record_fields", t.mutable_record_fields;
      "array_writes", t.array_writes;
      "function_calls", t.function_calls;
      "always_inline", t.always_inline;
      "never_inline", t.never_inline;
      "conditionals", t.conditionals;
      "bounded_loops", t.bounded_loops;
      "bitwise_operations", t.bitwise_operations;
      "opaque_initializers", t.opaque_initializers;
      "opaque_leaves", t.opaque_leaves;
      "opaque_loop_bounds", t.opaque_loop_bounds ]
    |> List.map (fun (name, enabled) -> Format.sprintf "  %s = %b" name enabled)
    |> String.concat "\n"

  let number_types t =
    List.filter
      (fun (ty : Ir.NumberTy.t) ->
        (t.floats || not (Ir.NumberTy.is_floating_point ty))
        && (t.unboxed_numbers || not ty.unboxed))
      Ir.NumberTy.all
end
