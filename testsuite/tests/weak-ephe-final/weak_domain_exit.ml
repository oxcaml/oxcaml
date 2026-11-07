(* TEST
 multicore;
 native;
*)

(* Regression test for #7864: weak arrays owned by domains that
   terminate. Exercises ephemeron orphaning and adoption in all major
   GC phases; failures show up as debug-runtime assertions (ephemeron
   round counters, orphan status), crashes, or a wedged major GC. *)

let key = Domain.DLS.new_key (fun () -> Weak.create 1)

let request n =
  Domain.DLS.set key (Weak.create 1);
  let w = Domain.DLS.get key in
  let keep = ref [] in
  for i = 1 to n do
    let r = ref i in
    Weak.set w 0 (Some r);
    keep := r :: !keep;
    if i mod 500 = 0 then keep := []
  done

let worker () =
  Gc.set { (Gc.get ()) with minor_heap_size = 64 * 1024 };
  for _ = 1 to 10 do request 5_000 done

let () =
  let rounds =
    let test_size =
      try int_of_string (Sys.getenv "OCAML_TEST_SIZE")
      with Not_found | Failure _ -> 0
    in
    if test_size >= 3 then 60 else 30
  in
  for _round = 1 to rounds do
    let d = Domain.spawn worker in
    let junk = ref [] in
    for i = 1 to 60_000 do
      junk := i :: !junk;
      if i mod 1000 = 0 then junk := []
    done;
    Domain.join d
  done;
  print_endline "ok"
