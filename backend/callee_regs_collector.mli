(** Collects the set of registers clobbered by leaf functions after register
    allocation and records the result in [Compilenv] for storage in .cmx
    files. *)

[@@@ocaml.warning "+a-40-41-42"]

(** [cfg cfg_with_layout] computes the set of registers clobbered by the
    function described by [cfg_with_layout] — if it is a leaf function —
    and records the result in [Compilenv].

    A leaf function is one that contains no [Call] or [Tailcall_func]
    terminators (i.e. it never transfers control to another OCaml function).
    Functions that allocate, poll, make external C calls, or contain probes
    are still considered leaf functions; their register effects are captured
    via [Proc.destroyed_at_basic] and [Proc.destroyed_at_terminator].

    Clobbered registers include registers appearing in the [res] arrays of
    all instructions, as well as registers implicitly destroyed by
    instructions (as reported by [Proc.destroyed_at_basic] and
    [Proc.destroyed_at_terminator]).

    This pass must run after all CFG rewrites (in particular after all calls
    to [simplify_cfg]) so that the collected set reflects what is actually
    emitted in the assembly output.

    If the function is not a leaf function, no information is recorded. *)
val cfg : Cfg_with_layout.t -> unit
