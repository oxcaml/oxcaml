let () =
  Api.reg_mod "Plugin_thread";
  (* Each line is written by one call: [print_endline] writes the newline
     separately, which lets the two threads interleave within a line. *)
  let t =
    Thread.create
      (fun () ->
         for i = 1 to 5 do
           print_string "Thread\n"; flush stdout;
           Thread.delay 0.1;
         done
      ) ()
  in
  for i = 1 to 10 do
    print_string "Thread\n"; flush stdout;
    Thread.delay 0.05;
  done;
  (* The program exits without waiting for its threads: wait here, so that
     the output does not depend on timing. *)
  Thread.join t
