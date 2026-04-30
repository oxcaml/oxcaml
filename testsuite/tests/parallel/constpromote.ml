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

(* when run with the bytecode debug runtime, this test
   used to trigger a bug where the constant [13]
   remained unpromoted *)

let rec burn l =
  if List.hd l > 14 then ()
  else burn (l @ l |> List.map (fun x -> x + 1))

let () =
  ignore (Domain.spawn (fun () -> burn [13]));
  burn [0];
  Printf.printf "all done\n%!"
