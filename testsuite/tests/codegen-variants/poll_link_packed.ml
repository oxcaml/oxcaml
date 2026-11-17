let () =
  assert (Poll_link_p.Poll_link_b.total [1; 2; 3] = 9);
  match Domain.spawn (fun () -> ()) with
  | domain ->
    Domain.join domain;
    Printf.printf "more than one domain\n%!"
  | exception Failure _ -> Printf.printf "one domain\n%!"
