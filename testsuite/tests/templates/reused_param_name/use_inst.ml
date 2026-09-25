module M = Use_p(P)(P_string) [@jane.non_erasable.instances]

let () = print_endline (M.go ())
