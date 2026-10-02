(** The call graph profile handed to the linker for function reordering: the
    SHT_LLVM_CALL_GRAPH_PROFILE section (".llvm.call_graph_profile"), which lld
    sorts the functions of the executable by (--call-graph-profile-sort, on by
    default). Its entries are (caller, callee, weight) with the caller and callee
    named by relocations against symbols, so that unknown symbols are skipped
    silently and the section never reaches the output.

    The edges come from the optimizing build ([Cfg_fdo_call_graph]): every real
    call instruction of a hot function, weighted by its block's count, to the
    functions the profile saw it reach, or to its static callee when the profile
    knows nothing about it. Callees the profile names by entry counter are
    referenced by an alias symbol ([alias_symbol]) that the callee's compilation
    unit defines at its entry when the profile knows the function, so that a
    function can be named without its symbol. *)

type callee =
  | Symbol of string  (** a function by symbol name *)
  | Entry of Fdo_counter.hashed  (** a function by its hashed entry counter *)

(** The alias symbol naming a function by its entry counter. *)
val alias_symbol : Fdo_counter.hashed -> Asm_targets.Asm_symbol.t

(** Record an edge of the current compilation unit; edges with the same
    endpoints are summed. *)
val add_edge : from:string -> callee:callee -> weight:int64 -> unit

val reset : unit -> unit

(** Emit the section for the current compilation unit and reset the state.
    Nothing is emitted when there are no edges. *)
val emit_section : unit -> unit
