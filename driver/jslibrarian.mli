(* Format of a library file:
      magic number (Config.cmja_magic_number)
      absolute offset of content table
      blocks of cmj bodies
      content table = list of compilation units
*)

val create_archive : string list -> string -> unit

type error = File_not_found of string | Not_an_object_file of string

exception Error of error

open Format

val report_error : formatter -> error -> unit
val reset : unit -> unit
