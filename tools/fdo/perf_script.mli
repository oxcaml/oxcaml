(** Streaming extraction of consecutive LBR samples from perf script output.
    Addresses are translated to the executable's link-time addresses. *)

type sample =
  { count : int64;
    ip : int64 option;
    branches : (int64 * int64) list  (** most recent first *)
  }

type executable =
  { path : string;
    address_of_offset : int64 -> int64 option
  }

val foreign : int64

(** With [executable], input includes mmap/task events and
    pid,period,ip,brstack. Otherwise it contains period,ip,brstack in link-time
    addresses (as produced by the single-step tracer). Call-chain lines are
    accepted but not used to infer context. Raises [Failure] on malformed input
    or missing mappings. *)
val iter_channel :
  ?executable:executable -> In_channel.t -> f:(sample -> unit) -> unit

val collect :
  executable:executable option -> perf_data:string -> f:(sample -> unit) -> unit
