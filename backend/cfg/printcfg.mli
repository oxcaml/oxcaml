[@@@ocaml.warning "+a-30-40-41-42"]

(** Terse rendering of a basic instruction description, with no register
    interleaving (suitable for error messages). *)
val basic_desc : Format.formatter -> Cfg.basic -> unit

(** Terse rendering of a terminator description, with no register interleaving
    (suitable for error messages). *)
val terminator_desc : ?sep:string -> Format.formatter -> Cfg.terminator -> unit

val print_basic' :
  ?print_reg:(Format.formatter -> Reg.t -> unit) ->
  Format.formatter ->
  Cfg.basic Cfg.instruction ->
  unit

val print_basic : Format.formatter -> Cfg.basic Cfg.instruction -> unit

val print_terminator' :
  ?print_reg:(Format.formatter -> Reg.t -> unit) ->
  Format.formatter ->
  Cfg.terminator Cfg.instruction ->
  unit

val print_terminator :
  Format.formatter -> Cfg.terminator Cfg.instruction -> unit

val print_instruction' :
  ?print_reg:(Format.formatter -> Reg.t -> unit) ->
  Format.formatter ->
  [ `Basic of Cfg.basic Cfg.instruction
  | `Terminator of Cfg.terminator Cfg.instruction ] ->
  unit

val print_instruction :
  Format.formatter ->
  [ `Basic of Cfg.basic Cfg.instruction
  | `Terminator of Cfg.terminator Cfg.instruction ] ->
  unit

val cfg : Format.formatter -> Cfg.t -> unit

val cfg_with_layout : Format.formatter -> Cfg_with_layout.t -> unit
