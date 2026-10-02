(** Each counting annotation specifies a complete within-function stack.
    Optional sharing against the previous stack in metadata order is purely an
    encoding: decoding a sample does not depend on earlier branch outcomes. *)

module Event : sig
  type t =
    | Branch of
        { taken : Fdo_counter.t list;
          fallthrough : Fdo_counter.t list
        }
    | Call of
        { return_address : Asm_targets.Asm_label.t;
          counter : Fdo_counter.t
        }
    | Tailcall of Fdo_counter.t
    | Jump of
        { target : Asm_targets.Asm_label.t;
          taken : Fdo_counter.t list
        }  (** an indirect jump, whose edge to [target] carries [taken] *)
    | Reset
end

(** The metadata of one compilation unit, accumulated function by function. *)
type t

(** [record_names] also records the canonical names behind the hashes, for
    readable decoder output. *)
val create : record_names:bool -> t

(** Record a compiled function's body hash, executed or not. *)
val record_body :
  t ->
  function_id:Fdo_counter.function_id ->
  function_body_hash:Fdo_counter.Function_body_hash.t ->
  unit

(** The metadata of one function, from [begin_function] to [end_function]. *)
type function_metadata

val begin_function :
  t ->
  start:Asm_targets.Asm_label.t ->
  entry_counters:Fdo_counter.t list ->
  function_metadata

val record_event :
  function_metadata -> Asm_targets.Asm_label.t -> Event.t -> unit

(** Adds the function to its compilation unit's metadata (unless nothing was
    recorded for it). *)
val end_function : function_metadata -> finish:Asm_targets.Asm_label.t -> unit

(** Emit the unit's "fdo_metadata" section. Nothing is emitted when no function
    was recorded and no body registered. *)
val emit_section : t -> unit
