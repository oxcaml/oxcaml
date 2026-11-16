(* TEST
 flags = "-trace-pc-guard";
 modules = "domains_stubs.c";
 linux;
 multicore;
 native;
*)

(* Coverage probes running concurrently in several domains, each of which has
   its own C stack for the callback. *)

external set_gate : bool -> unit = "caml_tpcg_domains_set_gate" [@@noalloc]

external num_calls : unit -> int = "caml_tpcg_domains_num_calls" [@@noalloc]

external num_bad_guards : unit -> int = "caml_tpcg_domains_num_bad_guards"
[@@noalloc]

let[@inline never] rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

let[@inline never] sum l =
  List.fold_left (fun acc (i, f) -> acc +. f +. Float.of_int i) 0. l

let[@inline never] work seed =
  let total = ref 0. in
  let l = ref [] in
  for i = 1 to 20_000 do
    l := (i, Float.of_int (i + seed)) :: !l;
    if i land 1023 = 0
    then (
      total := !total +. sum !l;
      l := [])
  done;
  let raised =
    try if seed land 1 = 0 then raise Exit else seed with Exit -> -1
  in
  Float.to_int !total + fib (15 + seed) + raised

let () =
  let seeds = [0; 1; 2; 3] in
  let expected = List.map work seeds in
  let calls_with_gate_closed = num_calls () in
  set_gate true;
  let domains =
    List.map
      (fun seed ->
        (Domain.spawn [@alert "-unsafe_multidomain-do_not_spawn_domains"])
          (fun () -> work seed))
      (List.tl seeds)
  in
  let here = work (List.hd seeds) in
  let actual = here :: List.map Domain.join domains in
  set_gate false;
  Printf.printf "calls with the gate closed: %d\n" calls_with_gate_closed;
  Printf.printf "callback called: %b\n" (num_calls () > 0);
  Printf.printf "bad guards: %d\n" (num_bad_guards ());
  Printf.printf "results preserved: %b\n" (List.equal Int.equal expected actual)
