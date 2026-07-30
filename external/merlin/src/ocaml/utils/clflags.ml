type profile_column = [ `Time | `Alloc | `Top_heap | `Abs_top_heap | `Counters ]
type shape_format = Old_merlin | Debugging_shapes
type visible_include =
  { path : string;
    cmx_guaranteed : bool;
  }

(** {0 OCaml compiler compatible command-line parameters} *)
let cmi_file = ref None
let include_dirs        = ref []
let hidden_include_dirs = ref []
let include_paths_files = ref ([] : string list) (* -I-paths *)
let hidden_include_paths_files = ref ([] : string list) (* -H-paths *)
let fast                = ref false
<<<<<<< Merlin:liam-merlin-5.4.0-ox4
||||||| Compiler:8fea84a50042cd6c3e05c8ef54e4b6970b72c783

(* Command-line parameters *)

module Int_arg_helper = Arg_helper.Make (struct
  module Key = struct
    include Numbers.Int
    let of_string = int_of_string
  end

  module Value = struct
    include Numbers.Int
    let of_string = int_of_string
  end
end)
module Float_arg_helper = Arg_helper.Make (struct
  module Key = struct
    include Numbers.Int
    let of_string = int_of_string
  end

  module Value = struct
    include Numbers.Float
    let of_string = float_of_string
  end
end)

type open_arg =
=======

(* Command-line parameters *)

(* Stripped down version of Numbers, as its dependencies are a lot of code *)
module Numbers = struct
  module Int = struct
    type t = int
    module Map = Map.Make(Stdlib.Int)
    let of_string = int_of_string
  end
  module Float = struct
    type t = float
    let of_string = float_of_string
  end
end

module Int_arg_helper = Arg_helper.Make (struct
  module Key = Numbers.Int

  module Value = Numbers.Int
end)
module Float_arg_helper = Arg_helper.Make (struct
  module Key = Numbers.Int

  module Value = Numbers.Float
end)

type open_arg =
>>>>>>> Compiler:d0ba5f3571676f89e2f535e9c3eb3a554c13f3aa
let classic             = ref false
let all_ppx             = ref []
let principal           = ref false
let real_paths          = ref true
let recursive_types     = ref false
let strict_sequence     = ref false
let applicative_functors = ref true

let nopervasives        = ref false
let strict_formats      = ref true

type open_arg =
  | Open of string
  | Open_cmi of string

let open_args           = ref ([] : open_arg list)
let parameters          = ref ([] : string list)
let as_parameter        = ref false
let as_argument_for     = ref None
let zero_alloc_check    = ref Zero_alloc_annotations.Check.Check_default
let zero_alloc_assert   = ref Zero_alloc_annotations.Assert.Assert_default
let infer_with_bounds   = ref false
let kind_verbosity = ref 0
let ikinds = ref true

let annotations         = ref false
let binary_annotations  = ref true
let binary_annotations_cms  = ref false
let shape_format        = ref Old_merlin
let store_occurrences   = ref true
let print_types         = ref false
let native_code         = ref false
let error_size          = ref 500
let dont_write_files    = ref true
let keep_locs           = ref true
let keep_docs           = ref false
let transparent_modules = ref true
let for_package         = ref None
let debug               = ref false
let unsafe              = ref false
let opaque              = ref false
let unboxed_types       = ref false
let profile_columns     = ref []
let dwarf_pedantic      = ref false
let gdwarf_config_shape_eval_depth = ref None
let gdwarf_config_max_type_to_shape_depth = ref None
let gdwarf_config_max_evaluation_steps_per_variable = ref None
let locs = ref true
let locations = ref true
let ikinds_debug = ref false
let no_alias_deps = ref false
let unique_ids = ref false
let dump_dir = ref None
let verbose_types = ref false
let canonical_ids = ref false
let print_variance = ref false
let error_style = ref (Some Misc.Error_style.Merlin)
