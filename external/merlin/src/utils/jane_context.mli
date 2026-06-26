(** The [Jane_context] represents whether Merlin is running within the context
    of Jane Street internal tooling. It can be set via the [MERLIN_JANE_CONTEXT]
    env var or by calling [set].

    Note that it cannot be loaded from config (like via .merlin files or dune)
    because one of the things it controls is how the config is found. *)

type t =
  | Vanilla  (** Represents Merlin running in a standard OCaml environment. *)
  | Jane_street
      (** Represents Merlin running in a Jane Street environment, where a
          JS-style dune and dune workspace is expected. *)

val get : unit -> t
val set : t -> unit
