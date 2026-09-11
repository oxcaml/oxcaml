[@@@ocaml.warning "+a-40-41-42"]

(** Checks that every block is reachable from the entry, against an independent
    Datalog specification. Fatal on failure; Z3 is used only to diagnose
    disagreements. *)
val validate_reachability : Cfg.t -> unit

(** Same check, using the internal Datalog engine only: returns [Error] instead
    of aborting, and never invokes Z3. Intended for fuzzing harnesses. *)
val check_reachability : Cfg.t -> (unit, string) result
