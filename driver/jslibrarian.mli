(* Format of a library file:
   .cmja files are now JavaScript archives created by js_of_ocaml link
   containing linked JavaScript code from multiple .cmjo files
*)

val create_archive : string list -> string -> unit

type error = File_not_found of string | Not_an_object_file of string

exception Error of error

open Format

val report_error : formatter -> error -> unit
val reset : unit -> unit
