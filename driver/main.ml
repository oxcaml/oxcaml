let () =
  exit (Profile.with_action_trace ~gettimeofday:Unix.gettimeofday ~name:"ocamlc"
    (fun () -> Maindriver.main Sys.argv Format.err_formatter))
