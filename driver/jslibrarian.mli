(* Format of library files:
   .cmja files are JavaScript archives created by js_of_ocaml link
   containing linked JavaScript code from multiple .cmjo files.
   .cmjxa files are metadata archives (like .cmxa) containing
   compilation unit information from .cmjx files.
*)

val create_archive : string list -> string -> unit

type error =
  | File_not_found of string
  | Archiver_error of string

exception Error of error

open Format

val report_error : formatter -> error -> unit
val reset : unit -> unit
