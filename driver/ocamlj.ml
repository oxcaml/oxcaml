let () =
  exit (Profile.record_action ~gettimeofday:Unix.gettimeofday ~name:"ocamlj"
    (fun () -> Jsmaindriver.main Sys.argv Format.err_formatter))
