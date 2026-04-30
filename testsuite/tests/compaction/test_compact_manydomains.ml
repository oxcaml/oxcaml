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

let num_domains = 20

let go () =
  let n = 50_000 in
  let c = Array.make n None in
  for i = 0 to n-1 do
    c.(i) <- Some (i, i)
  done;
  Gc.compact ()

let () =
  Array.init num_domains (fun _ -> Domain.spawn go)
  |> Array.iter Domain.join
