external hit_cstub : unit -> unit = "hit_cstub" [@@noalloc]

module M = Sancov_lib.Make (struct
  let limit = 5
end)

let () =
  let n = Sys.opaque_identity 3 in
  Sancov_lib.hit_loop (n + 7);
  Sancov_lib.hit_search [| 1; 2; 3 |] n;
  Sancov_lib.hit_classify (n - 3);
  Sancov_lib.hit_classify (n - 1);
  M.hit_functor n;
  Sancov_lib.hit_caller n;
  Sancov_lib.hit_rec n;
  List.iter (fun x -> if x = n then Sancov_lib.hit_closure ()) [1; 2; 3];
  hit_cstub ();
  if Sys.opaque_identity false
  then Sancov_lib.missed_iter (fun _ -> Sancov_lib.missed_closure ())
