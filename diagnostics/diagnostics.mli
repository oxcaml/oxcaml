val diagnostic_of_exception :
  Location.report -> exn -> Structured_diagnostic.t option

val enable_structured_diagnostics : unit -> unit
