type problem =
  | Unreadable of { path : string; message : string }
  | Malformed of { path : string; message : string }

type status =
  { facts_present : bool;
    channels_loaded : int;
    sources_folded : int;
    problems : problem list
  }

val pp_problem : Format.formatter -> problem -> unit

val flush : ?older_than:float -> unit -> unit

val fold :
  index_files:string list ->
  init:'acc ->
  f:('acc -> path:string -> Module_implementation_facts.t -> 'acc) ->
  'acc * status

val load : index_files:string list -> Module_implementation_facts.t * status
