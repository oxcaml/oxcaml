(* TEST
 runtime5;
 multidomain;
 native;
*)

[@@@alert "-do_not_spawn_domains"]

external caml_callback : ('a -> 'b) -> 'a -> 'b = "caml_callback"

let callback_minor () = caml_callback Gc.minor ()

let make_dead_cont () =
  Effect.Deep.match_with callback_minor () {
    retc = ignore;
    exnc = raise;
    effc = fun _ -> None
  }

let () =
  let _ = Domain.Safe.spawn (fun () ->
    while true do Gc.compact () done) in
  let mutable state = "hello" in
  for _ = 0 to 10_000 do
    make_dead_cont ();
    state <- "world";
  done;
  ignore state
