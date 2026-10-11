[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]

(** Sinking (partial dead code elimination): move operations towards their uses,
    into blocks that are not executed on every path, so that they are only
    computed on the paths that need them. *)
val run :
  ppf_dump:Format.formatter -> Ssa.finished Ssa.graph -> Ssa.finished Ssa.graph
