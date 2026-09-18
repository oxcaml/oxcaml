[@@@ocaml.warning "+a-40-41-42"]

(** Prints "res := " if [res] is non-empty, otherwise nothing. *)
val result_prefix :
  ?print_reg:(Format.formatter -> Reg.t -> unit) ->
  Format.formatter ->
  Reg.t array ->
  unit

(** Prints the operation and its arguments (with arguments interleaved in the
    operation's rendering), without any prefix for results. *)
val operation_body :
  ?print_reg:(Format.formatter -> Reg.t -> unit) ->
  Operation.t ->
  Reg.t array ->
  Format.formatter ->
  unit

(** Prints "res := <op with interleaved args>". Equivalent to calling
    [result_prefix] followed by [operation_body]. *)
val operation :
  ?print_reg:(Format.formatter -> Reg.t -> unit) ->
  Operation.t ->
  Reg.t array ->
  Format.formatter ->
  Reg.t array ->
  unit
