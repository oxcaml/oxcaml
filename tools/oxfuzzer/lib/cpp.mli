(** An independent C++17 reference lowering of the generator IR. Numeric
    representations are erased; observable values, sharing and effects are not.
    The result is a self-contained translation unit. *)
val to_code : Program.t -> string
