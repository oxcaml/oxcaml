(* TEST
 no-stack-checks;
 no-address-sanitizer;
 native;
*)

(* Suspended continuations that survive a minor collection no longer hold
   onto their stack mappings (their stacks are "idled": copied into
   malloced memory), so leaking many of them must not exhaust the kernel
   mapping limit. This program used to die with Out_of_fibers after a few
   tens of thousands of iterations. Where /proc is available, also check
   that the mapping count stays bounded (by the guarded-stack cache)
   instead of growing with the number of suspended fibers, which would
   need ~3 mappings each. *)

type _ Effect.t += Leak : unit Effect.t

(* The process's mapping count, or None where /proc is unavailable. *)
let count_maps () =
  match open_in "/proc/self/maps" with
  | exception Sys_error _ -> None
  | ic ->
    let n = ref 0 in
    (try
       while true do
         ignore (input_line ic);
         incr n
       done
     with End_of_file -> ());
    close_in ic;
    Some !n

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
  let baseline = count_maps () in
  for i = 1 to 100_000 do
    Effect.Deep.match_with (fun () -> Effect.perform Leak) () handler;
    if i mod 1000 = 0 then Gc.minor ()
  done;
  Gc.minor ();
  (match baseline, count_maps () with
  | Some baseline, Some peak when peak - baseline >= 20_000 ->
    Printf.printf "mapping count grew by %d\n" (peak - baseline)
  | _ -> print_endline "mappings bounded");
  (* And they can all still be woken and run to completion. *)
  List.iter (fun k -> Effect.Deep.continue k ()) !leaked_stacks;
  print_endline "ok"
