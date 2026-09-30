(** Complete within-function stacks and dynamic cross-function context. *)

type t

(** Parse versioned fragments and expand metadata-order stack sharing. Call
    lengths determine their return PCs. Reject malformed data and conflicting
    names for a hash (when names are present). *)
val parse : string -> t

val names : t -> string Fdo_counter.Hash.Tbl.t

(** Print the annotations of each function in metadata order, and the body
    hashes, by name where names are present. Code addresses and offsets are
    omitted, so that the output does not depend on code layout. *)
val print : Format.formatter -> t -> unit

(** The body hashes of every compiled instrumented function, by function hash.
*)
val bodies : t -> Fdo_counter.Function_body_hash.t Fdo_counter.Hash.Tbl.t

(** The decisions [process_sample] takes, for tracing it. *)
module Trace_event : sig
  type t =
    | Branch of
        { source : int64;
          target : int64
        }  (** the next branch of the sample, before its effects *)
    | Count of Fdo_counter.hashed
        (** a complete stack (static, then dynamic context) is counted *)
    | Call of Fdo_counter.hashed  (** a caller frame is pushed *)
    | Tailcall of Fdo_counter.hashed
        (** context is extended, without a return address *)
    | Return  (** a RET unwinds to the frame of its return address *)
    | Reset  (** an exception handler discards the dynamic context *)
    | Skip_call  (** a call whose source or destination is unavailable *)
    | Discard_context  (** unavailable code loses the dynamic context *)
end

(** Interpret one consecutive LBR sample, starting with unknown calling context.
    Branches are most recent first. The oldest branch only sets up the state:
    its own annotations change the dynamic context but count nothing, and its
    target starts the first straight-line range, so that samples overlapping by
    one branch count everything once. Each traversed instruction selects
    on_fallthrough or on_taken annotations; annotations with both bits set run
    in either case. Every counting annotation supplies its entire static stack,
    even if earlier instructions are absent from the sample. Calls and tail
    calls are processed only when their source and destination code is
    available. The edges of an indirect jump count only when the branch lands at
    their target. An x86 near RET at the branch source unwinds to a matching
    saved return PC; an ordinary jump to the same address does not. Reset and
    unavailable-code gaps discard caller context. Instrumented calls may pass
    through opaque stubs within the available image. [code_byte] reads
    executable bytes at link-time addresses, returning None when unavailable. *)
val process_sample :
  ?trace:(Trace_event.t -> unit) ->
  t ->
  code_byte:(int64 -> int option) ->
  branches:(int64 * int64) list ->
  f:(Fdo_counter.hashed -> unit) ->
  unit

(** A printer of the trace events of one sample. Code is named by the entry
    counter of the function containing it (or "?"), without addresses; runs of
    branches in unannotated code print as one line. *)
val trace_printer : t -> Format.formatter -> Trace_event.t -> unit
