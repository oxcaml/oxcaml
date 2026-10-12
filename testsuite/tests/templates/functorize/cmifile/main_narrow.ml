module R = Bundle_narrow.Make (P_int) ()

let () = print_endline (R.Basic.to_string (R.Basic.create (P_int.create ())))
