type t =
  { version : int;
    uids : Shape.Uid.t array;
    units : Compilation_unit.t array;
    files : string array;
    context_count : int;
    contexts : string;
    key_count : int;
    keys : string;
    checks : string;
    dependencies : string;
    equalities : string;
    omissions : string
  }

exception Malformed of string

val version : int

val max_context_depth : int

val max_context_expanded_size : int

val max_table_entries : int

val max_fact_rows : int

val max_decoded_budget_bytes : int64

module Decoded_budget : sig
  type t

  val create : unit -> t

  val charge_table_entries : t -> what:string -> count:int -> unit

  val charge_fact_rows : t -> what:string -> count:int -> unit
end

module For_testing : sig
  val decode_canonical_uint :
    max_native_int:int64 -> string -> (int64, string) result
end

val empty : t

val is_empty : t -> bool

val of_facts : Module_implementation_facts.t -> t

val to_facts : t -> (Module_implementation_facts.t, string) result
