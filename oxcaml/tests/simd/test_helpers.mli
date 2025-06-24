type rosetta2_status =
  | Unknown
  | Off
  | On

val run_if_not_under_rosetta2 : f:(unit -> unit) -> unit
