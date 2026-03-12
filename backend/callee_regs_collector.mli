(** Collects the set of registers clobbered by leaf functions after register
    allocation and records the result in [Compilenv] for storage in .cmx
    files. *)

[@@@ocaml.warning "+a-40-41-42"]

(** [cfg cfg_with_infos] computes the set of registers clobbered by the
    function described by [cfg_with_infos] — if it is a leaf function
    (i.e. [fun_contains_calls = false]) — and records the result in
    [Compilenv].

    Clobbered registers include registers appearing in the [res] arrays of
    all instructions, as well as registers implicitly destroyed by
    instructions (as reported by [Proc.destroyed_at_basic] and
    [Proc.destroyed_at_terminator]).

    If the function is not a leaf function, no information is recorded. *)
val cfg : Cfg_with_infos.t -> unit
