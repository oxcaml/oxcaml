type t =
  | Vanilla  (** Represents Merlin running in a standard OCaml environment. *)
  | Jane_street
      (** Represents Merlin running in a Jane Street environment, where a JS-style
          dune and dune workspace is expected. *)

val current : t
