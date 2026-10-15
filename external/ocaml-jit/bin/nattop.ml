let () =
  Clflags.native_code := true;
  Opttoploop.set_paths ();
  Opttoploop.initialize_toplevel_env ();
  Clitop.main ~name:"nattop" ~eval_phrase:Opttoploop.execute_phrase
    ~loop:Opttoploop.loop ()
