(* TEST
   runtime5;
   flags = "-alert -unsafe_multidomain -alert -do_not_spawn_domains";
   multidomain;
   { native; }
   { bytecode; }
*)

let print_interval ?expected () =
  let interval = Domain.Tick.global_effective_interval_usec () in
  (match interval with
   | Null -> Printf.printf "Global (effective) interval: none"
   | This n -> Printf.printf "Global (effective) interval: %d μs" n);
  (match expected with
   | None -> ()
   | Some None -> Printf.printf "(expected none)"
   | Some (Some n) -> Printf.printf "(expected %d μs)" n);
  Printf.printf "\n"

let single_domain () =
  print_endline "Single domain:";
  print_interval
    ~expected:None
    ();
  print_endline "acquiring tick at 100μs";
  let tick_100us_1 = Domain.Tick.acquire ~interval_usec:100 in
  print_interval
    ~expected:(Some 100)
    ();
  print_endline "acquiring tick at 50μs";
  let tick_50us = Domain.Tick.acquire ~interval_usec:50 in
  print_interval
    ~expected:(Some 50)
    ();
  print_endline "acquiring second tick at 100μs";
  let tick_100us_2 = Domain.Tick.acquire ~interval_usec:100 in
  print_interval
    ~expected:(Some 50)
    ();
  print_endline "Releasing 50μs tick";
  Domain.Tick.release tick_50us;
  print_interval
    ~expected:(Some 100)
    ();
  print_endline "releasing 100μs tick";
  Domain.Tick.release tick_100us_2;
  print_interval
    ~expected:(Some 100)
    ();
  print_endline "releasing 100μs tick";
  Domain.Tick.release tick_100us_1;
  print_interval
    ~expected:None
    ()

let multi_domain () =
  print_endline "\nMulti domain:";
  let step = Atomic.make 0 in
  let wait_for_step n =
    while Atomic.get step < n do Domain.cpu_relax () done
  in
  let advance_step () =
    ignore (Atomic.fetch_and_add step 1)
  in
  (* Main acquires 200μs *)
  let tick_200 = Domain.Tick.acquire ~interval_usec:200 in
  print_endline "Main acquired 200μs";
  print_interval
    ~expected:(Some 200)
    ();
  (* Child acquires 100μs while main still holds 200μs *)
  let d = Domain.spawn (fun () ->
    let tick_100 = Domain.Tick.acquire ~interval_usec:100 in
    advance_step (); (* step -> 1: child has acquired *)
    wait_for_step 2;
    (* Main has checked effective=100. Now child acquires 50μs too *)
    let tick_50 = Domain.Tick.acquire ~interval_usec:50 in
    advance_step (); (* step -> 3: child has both 100 and 50 *)
    wait_for_step 4;
    (* Main has checked effective=50. Child releases 50μs *)
    Domain.Tick.release tick_50;
    advance_step (); (* step -> 5: child back to just 100 *)
    wait_for_step 6;
    (* Main has checked effective=100. Child releases 100μs *)
    Domain.Tick.release tick_100;
    advance_step (); (* step -> 7: child has no ticks *)
    wait_for_step 8
  ) in
  (* Both domains have ticks: main=200, child=100 *)
  wait_for_step 1;
  print_endline "Child acquired 100μs";
  print_interval
    ~expected:(Some 100)
    ();
  advance_step (); (* step -> 2 *)
  (* Both domains have ticks: main=200, child=min(100,50)=50 *)
  wait_for_step 3;
  print_endline "Child also acquired 50μs";
  print_interval
    ~expected:(Some 50)
    ();
  advance_step (); (* step -> 4 *)
  (* Both domains have ticks: main=200, child=100 *)
  wait_for_step 5;
  print_endline "Child released 50μs";
  print_interval
    ~expected:(Some 100)
    ();
  advance_step (); (* step -> 6 *)
  (* Main still has 200, child has released everything *)
  wait_for_step 7;
  print_endline "Child released 100μs";
  print_interval
    ~expected:(Some 200)
    ();
  advance_step (); (* step -> 8 *)
  Domain.join d;
  Domain.Tick.release tick_200;
  print_endline "Main released 200μs";
  print_interval
    ~expected:None
    ()

let () =
  single_domain ();
  multi_domain ()
