(* TEST flags = "-w -71"; *)

<<<<<<< HEAD
module Test1 = struct
  let[@tail_mod_cons] rec f () = Some (g ())
  and[@tail_mod_cons] g () = false && true

  let () = assert (f () = Some false)
end

module Test2 = struct
  let[@tail_mod_cons] rec f () = Some (g ())
  and[@tail_mod_cons] g () = true || false

  let () = assert (f () = Some true)
end
||||||| 23e84b8c4d
=======
let[@tail_mod_cons] rec f () = Some (g ())
and[@tail_mod_cons] g () = false && true

let () = assert (f () = Some false)

let[@tail_mod_cons] rec f () = Some (g ())
and[@tail_mod_cons] g () = true || false

let () = assert (f () = Some true)
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
