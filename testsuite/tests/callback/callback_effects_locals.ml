(* TEST
 modules = "callback_effects_locals_.c";
 runtime5;
 native;
*)
type 'a box = { unbox: 'a @@ global } [@@boxed]

external callback : 'a box @ local -> (unit -> unit) -> 'a = "cb"

let wrap_callback ch f =
  callback (stack_ {unbox=Array.init 5 (fun i -> String.make (Sys.opaque_identity i) ch)}) f
  |> Array.iter print_endline

let wrap_fiber f =
  Effect.Deep.try_with f () { effc = fun _ -> None }

let f () =
  Gc.minor ();
  wrap_callback 'c' (fun () ->
    wrap_fiber (fun () ->
      wrap_callback 'b' (fun () ->
        wrap_fiber (fun () ->
          wrap_callback 'a' (fun () ->
            wrap_fiber (fun () ->
              Gc.minor ();
              List.init 1000 ref |> Sys.opaque_identity |> ignore))))))

let () = (Sys.opaque_identity f) ()
