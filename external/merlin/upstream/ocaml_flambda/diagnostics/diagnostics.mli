val diagnostic_of_exception :
  legacy:Location.report -> exn -> Structured_diagnostic.t

val enable_structured_diagnostics : unit -> unit

val enable_json : unit -> unit
