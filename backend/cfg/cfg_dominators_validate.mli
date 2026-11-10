[@@@ocaml.warning "+a-40-41-42"]

(** Checks immediate dominators against an independent Datalog specification. Z3
    is used only to diagnose disagreements. *)

val validate_idom : Cfg.t -> Label.t Label.Tbl.t -> unit

module For_testing : sig
  (** Same check as [validate_idom], using the internal Datalog engine only:
      returns [Error] instead of aborting, and never invokes Z3. Intended for
      fuzzing harnesses. *)
  val check_idom : Cfg.t -> Label.t Label.Tbl.t -> (unit, string) result
end
