(** The body hash of a function for pseudo-instrumentation counters (see
    [Fdo_counter]).

    Counters identify the branching constructs and call sites of a function by
    their index in translation order, so that a profile can be applied across
    code edits elsewhere. The indices only mean the same thing while the
    function is unchanged: the body hash is a structural hash of its whole
    Lambda body (constructors, constants, switch keys, identifiers by name, and
    primitives by name, field index, block tag, C function or global). It leaves
    out what varies between compilations of the same code or does not affect the
    numbering: source positions, identifier stamps, static handler numbers,
    layouts, modes, attributes and debugger events. *)

(** The body hash of a function with the given parameters and body. *)
val of_function :
  params:Ident.t list -> Lambda.lambda -> Fdo_counter.Function_body_hash.t

(** The first (up to five) distinct tokens of a function: its parameters, then,
    in translation order, the identifiers its body binds or mentions, the values
    of other compilation units it uses (unit name and offset), its primitives
    (with their tag or offset: "field0", "block1", "+", "caml_hash") and its
    integer constants. They name anonymous functions in counters: readable, and
    unchanged by edits further into the body. *)
val leading_tokens : params:Ident.t list -> Lambda.lambda -> string list
