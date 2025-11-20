(* TEST
 runtime5;
 no-stack-checks;
 no-address-sanitizer;
 exit_status = "2";
 native;
*)

type _ Effect.t += Leak : unit Effect.t

let () =
  Printexc.record_backtrace false;
  let leaked_stacks = ref [] in
  let effc (type a) (e : a Effect.t) =
    match e with
    | Leak ->
      Some (fun (k : (a, unit) Effect.Deep.continuation) ->
        leaked_stacks := (k : (unit, unit) Effect.Deep.continuation) :: !leaked_stacks)
    | _ ->
      None
  in
  let handler = { Effect.Deep.retc = Fun.id; exnc = raise; effc } in
  for _ = 1 to 1_000_000 do
    Effect.Deep.match_with (fun () -> Effect.perform Leak) () handler
  done
