(* TEST
 runtime5;
 multidomain;
 { bytecode; }
 { native; } *)
let sink = ref []

let () =
  let domain =
    (Domain.spawn[@alert "-unsafe_multidomain-do_not_spawn_domains"]) (fun () ->
      for i = 1 to 10_000 do
        sink := [| i; i + 1; i + 2; i + 3 |] :: !sink
      done;
      ignore (Sys.opaque_identity sink);
      let quick = Gc.quick_stat () in
      let direct_minor_words = Gc.minor_words () in
      Printf.printf
        "child_index=%d quick_minor_kwords=%.0f direct_minor_kwords=%.0f\n%!"
        (Domain.self_index ())
        (quick.Gc.minor_words /. 1000.)
        (direct_minor_words /. 1000.))
  in
  Domain.join domain
