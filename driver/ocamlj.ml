let () =
  exit (Profile.with_action_trace ~gettimeofday:Unix.gettimeofday ~name:"ocamlj"
    (fun () -> Jsmaindriver.main Sys.argv Format.err_formatter))
