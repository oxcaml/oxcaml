let () =
  exit (Profile.record_action ~gettimeofday:Unix.gettimeofday ~name:"ocamlc"
    (fun () -> Maindriver.main Sys.argv Format.err_formatter))
