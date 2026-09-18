let () =
  Toploop.set_paths ();
  Toploop.initialize_toplevel_env ();
  Clitop.main ~name:"bytetop" ~eval_phrase:Toploop.execute_phrase
    ~loop:Toploop.loop ()
