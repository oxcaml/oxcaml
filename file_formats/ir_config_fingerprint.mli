(* Fingerprint of the process configuration that saved IR depends on.

   [.cmir-linear] and [.cmir-cfg] files bake configuration-dependent code
   generation decisions into the saved instructions, while later stages
   (e.g. Emit) consult the configuration of the process that reloads the
   file. A fingerprint of the relevant configuration is therefore stored
   when saving, and checked when reloading. *)

type t

val current : unit -> t

(* A configuration item on which the saving and reloading processes
   disagree; values are rendered as strings for error reporting. *)
type mismatch =
  { name : string;
    saved_value : string;
    current_value : string;
  }

(* The payload of the error raised when a file cannot be reloaded because
   its fingerprint disagrees with the current process. *)
type configuration_mismatch =
  { filename : string;
    entries : mismatch list;
  }

val print_configuration_mismatch : configuration_mismatch Format_doc.printer

(* [read_and_check ic ~filename ~raise_configuration_mismatch] reads the
   fingerprint stored in [ic] (as written by [current]) and compares it with
   the current process's configuration; on any disagreement it calls
   [raise_configuration_mismatch], which is expected to raise. *)
val read_and_check :
  in_channel ->
  filename:string ->
  raise_configuration_mismatch:(configuration_mismatch -> unit) ->
  unit
