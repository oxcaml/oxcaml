(* Compiled against [other/dep.ml]. Harmless if it is run all the same: it
   only reads [Dep.v]. *)
let () = ignore (Dep.v : string); print_endline "Bad_user was run"
