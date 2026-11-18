val record : string -> unit

type state = { mutable value : int }

val counter : int ref
val state : state ref
val slot : (unit -> int) option ref
val factory : (unit -> int) option ref
