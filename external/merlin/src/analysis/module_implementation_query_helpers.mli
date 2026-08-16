val impl_source_of_interface : Mconfig.t -> string -> string option

val module_facts :
  Mconfig.t -> Module_implementation_facts.t * Module_facts_reader.status

val own_file : Mconfig.t -> string

val find_source_of_loc :
  Mconfig.t -> description:string -> Location.t -> (string * Location.t) option

val location_in_file : string -> Location.t -> Location.t
