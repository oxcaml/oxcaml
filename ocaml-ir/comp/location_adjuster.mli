module Location = Ocaml_ir_common.Location
type t
val map : from:Warnings.loc -> t -> Warnings.loc
val map' : from:Location.Simple.t -> t -> Location.Simple.t
val create : unit -> t
val default_for_fundecl :
  file_content:string -> Parsetree.structure option -> t -> unit
val default_for_call :
  file_content:string -> Parsetree.structure option -> t -> unit
val shrink_for_datatypes :
  file_content:string -> Parsetree.structure option -> t -> unit
