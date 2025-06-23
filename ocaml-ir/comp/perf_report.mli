open Ocaml_ir_common
type summary
val summary_of_sexp : Sexplib.Sexp.t -> summary
val sexp_of_summary : summary -> Sexplib.Sexp.t
val summarize_perf_data :
  ?force_recompute:bool ->
    ?buildid_cache_dir:string ->
      ?binary:string -> string -> summary Result.Fiber.t
open Perf_counters
val events_in_file :
  filename:string -> event:Event_type.t -> summary -> Events_for_a_file.t
val event_types_in_project : summary -> Event_types_in_project.t
val event_count_per_files :
  event:Event_type.t -> summary -> Event_count_per_files.t
