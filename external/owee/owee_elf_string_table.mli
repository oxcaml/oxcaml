(** ELF string table builder.

    This module provides a mutable builder for constructing ELF string tables.
    String tables in ELF are sequences of null-terminated strings, where each
    string is referenced by its byte offset from the start of the table. *)

(** A mutable string table builder. *)
type t

(** [create ()] creates a new empty string table builder.
    The table is initialized with a null byte at offset 0, as required by ELF. *)
val create : unit -> t

(** [add t s] adds string [s] to the table if not already present and returns
    its offset. If [s] was already added, returns the existing offset. *)
val add : t -> string -> int

(** [length t] returns the current length of the string table in bytes. *)
val length : t -> int

(** [contents t] returns the string table contents as bytes. *)
val contents : t -> bytes
