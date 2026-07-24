val run_z3 : string -> string

module Id_gen : sig
  type t
  val create : Cfg.t -> t
  val get_id : t -> label:Label.t -> string
  val width : t -> int
  val label_of_id : t -> int -> Label.t
end

val z3_graph_of_cfg : Format.formatter -> cfg:Cfg.t -> id_gen:Id_gen.t -> unit

val fmt_dom_code_begin : Format.formatter -> id_gen:Id_gen.t -> unit
val fmt_dom_code_end : Format.formatter -> unit

val parse_doms :
  id_gen:Id_gen.t ->
  entry_label:Label.t ->
  string ->
  Label.t Label.Tbl.t
