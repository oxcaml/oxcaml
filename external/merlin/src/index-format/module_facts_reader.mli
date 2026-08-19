val flush : ?older_than:float -> unit -> unit

val fold :
  index_files:string list ->
  init:'acc ->
  f:('acc -> path:string -> Module_implementation_facts.t -> 'acc) ->
  'acc
