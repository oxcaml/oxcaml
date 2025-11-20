let () =
  Clflags.native_code := true;
  Opttoploop.set_paths ();
  Opttoploop.initialize_toplevel_env ();
  Jit.init_top ();
  Clitop.main ~name:"jittop" ~eval_phrase:Opttoploop.execute_phrase
    ~loop:Opttoploop.loop ()
