let check_that_types_are_the_same (aliased_t : Reexport.As_alias.t)
    (included_t : Reexport.Included.t) : P.t =
  if true then aliased_t else included_t

let check_that_we_can_locate_things () =
  let _ = Reexport.As_alias.create () in
  let _ = Reexport.Included.create () in
  ()
