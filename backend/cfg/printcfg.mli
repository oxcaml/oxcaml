[@@@ocaml.warning "+a-30-40-41-42"]

(** Terse rendering of a basic instruction description, with no register
    interleaving and no colour wrapping (suitable for error messages). *)
val basic_desc : Format.formatter -> Cfg.basic -> unit

(** Terse rendering of a terminator description, with no register interleaving
    and no colour wrapping (suitable for error messages). *)
val terminator_desc : ?sep:string -> Format.formatter -> Cfg.terminator -> unit

(** Renders the body of a basic instruction (no result, no instruction id, no
    surrounding colour wrapping). Register arguments are interleaved into the
    operation syntax for the [Op] case, and printed via [print_reg]. *)
val basic_body :
  ?print_reg:(Format.formatter -> Reg.t -> unit) ->
  Format.formatter ->
  Cfg.basic Cfg.instruction ->
  unit

(** Renders the body of a terminator instruction (no result, no instruction id,
    no surrounding colour wrapping). Register arguments are interleaved into the
    terminator syntax, and printed via [print_reg]. *)
val terminator_body :
  ?print_reg:(Format.formatter -> Reg.t -> unit) ->
  ?sep:string ->
  Format.formatter ->
  Cfg.terminator Cfg.instruction ->
  unit

(** Full rendering of a basic instruction (result, body) with colour wrapping.
*)
val basic : Format.formatter -> Cfg.basic Cfg.instruction -> unit

(** Full rendering of a terminator instruction (result, body) with colour
    wrapping. *)
val terminator : Format.formatter -> Cfg.terminator Cfg.instruction -> unit

(** Full rendering of an instruction (basic or terminator) with colour wrapping.
*)
val instruction :
  Format.formatter ->
  [ `Basic of Cfg.basic Cfg.instruction
  | `Terminator of Cfg.terminator Cfg.instruction ] ->
  unit

(** Variant of {!basic} taking a custom register printer. *)
val basic_with_print_reg :
  print_reg:(Format.formatter -> Reg.t -> unit) ->
  Format.formatter ->
  Cfg.basic Cfg.instruction ->
  unit

(** Variant of {!terminator} taking a custom register printer. *)
val terminator_with_print_reg :
  print_reg:(Format.formatter -> Reg.t -> unit) ->
  Format.formatter ->
  Cfg.terminator Cfg.instruction ->
  unit

(** Variant of {!instruction} taking a custom register printer. *)
val instruction_with_print_reg :
  print_reg:(Format.formatter -> Reg.t -> unit) ->
  Format.formatter ->
  [ `Basic of Cfg.basic Cfg.instruction
  | `Terminator of Cfg.terminator Cfg.instruction ] ->
  unit

(** Full rendering of a CFG (blocks, instructions, edges) with colour wrapping.
    Blocks are visited in [Label.compare] order. *)
val cfg : Format.formatter -> Cfg.t -> unit

(** Like {!cfg}, but visits blocks in the layout's order. *)
val cfg_with_layout : Format.formatter -> Cfg_with_layout.t -> unit
