(* TEST
<<<<<<< HEAD
 multicore;
||||||| 5.2.0minus-31
 flags += "-alert -do_not_spawn_domains -alert -unsafe_multidomain";
 runtime5;
 multidomain;
=======
 runtime5;
 multidomain;
>>>>>>> 5.2.0minus-37
 { bytecode; }
 { native; }
*)

let _ =
  let key_array =
    Array.init 128 (fun i -> Domain.DLS.new_key (fun _ -> i))
  in
  assert (Domain.DLS.get (key_array.(42)) = 42);
  let d = Domain.spawn (fun _ ->
    assert (Domain.DLS.get (key_array.(63)) = 63))
  in
  Domain.join d;
  print_endline "OK"
