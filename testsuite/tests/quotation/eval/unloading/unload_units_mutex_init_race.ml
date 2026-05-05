(* TEST
  modules = "unload_units_mutex_init_race_.c";
  runtime5;
  no-address-sanitizer;
  { native; }
*)

external register_unregister_dummy : unit -> unit
  = "caml_test_register_unregister_dummy_unloadable_unit"

let[@inline never] run_one () =
  for _ = 1 to 1000 do
    register_unregister_dummy ()
  done

let () =
  let num_domains =
    let n = Domain.recommended_domain_count () in
    max 1 (min 8 n)
  in
  let start = Atomic.make false in
  let domains =
    Array.init (num_domains - 1) (fun _ ->
        Domain.spawn (fun () ->
            while not (Atomic.get start) do
              Domain.cpu_relax ()
            done;
            run_one ()))
  in
  Atomic.set start true;
  run_one ();
  Array.iter Domain.join domains;
  print_endline "ok"
