let f (module M : Instantiate_empty.S) =
  Instantiate_aux.label ^ " " ^ string_of_int (M.z + Instantiate_p.x)
