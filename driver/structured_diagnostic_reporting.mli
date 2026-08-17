val flag : string
val setup : string array -> unit
val report_exception : Format.formatter -> exn -> unit
val report_message : ?usage:(unit -> unit) -> string -> unit
