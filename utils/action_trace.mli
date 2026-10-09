(** If the [DUNE_ACTION_TRACE_DIR] environment variable is set, this module will
    write event data to json files in the directory pointed at by
    [DUNE_ACTION_TRACE_DIR]. This can be used by the build system to log event
    data from the compiler. *)

type counters := (string * int) list

val write_instant :
  ?args:(string * Json.t) list ->
  ?counters:counters ->
  name:string ->
  time_in_nanoseconds:int ->
  unit ->
  unit

val write_span :
  ?args:(string * Json.t) list ->
  ?counters:counters ->
  name:string ->
  start_in_nanoseconds:int ->
  finish_in_nanoseconds:int ->
  unit ->
  unit
