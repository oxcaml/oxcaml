val main :
  name:string ->
  eval_phrase:(bool -> Format.formatter -> Parsetree.toplevel_phrase -> bool) ->
  loop:(Format.formatter -> unit) ->
  unit ->
  unit
