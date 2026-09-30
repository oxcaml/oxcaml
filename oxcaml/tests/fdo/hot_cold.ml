(* hot_cold_profile.txt names [f]'s body hash and branch index.
   Update the profile when changing its body; source positions do not matter. *)

let[@inline never] hot () = print_string "hot"

let[@inline never] cold () = print_string "cold"

let[@inline never] f b =
  if b
  then hot ()
  else cold ()

let () = f (Array.length Sys.argv > 1)
