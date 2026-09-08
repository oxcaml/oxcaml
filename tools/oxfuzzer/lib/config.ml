(* CR-someday hwasilewski: Move all constants, including probabilities, into
   Config. *)
(* CR-soon hwasilewski: Make [Config] controlled by swarm testing. *)
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
