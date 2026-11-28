(* TEST
 multicore;
 native;
*)

(* Regression test: dead weak arrays (trivial-data ephemerons) are
   moved to the live list unmarked by ephe_mark's fast path; a domain
   exiting mid-marking must darken them before orphaning, or adopters
   see unmarked ephemerons on the orphan list (debug-runtime assertion
   in orph_ephe_list_verify_status). See ocaml/ocaml#14722. *)

let worker () =
  Gc.set { (Gc.get ()) with minor_heap_size = 64 * 1024 };
  (* Weak arrays that immediately become garbage, so they are unmarked
     when ephe_mark's trivial-data path visits them. *)
  for _ = 1 to 2_000 do
    ignore (Sys.opaque_identity (Weak.create 8))
  done;
  (* Drive this domain's own major slices so marking starts and
     ephe_mark processes the todo list, then exit, ideally still
     mid-marking. *)
  for _ = 1 to 20 do
    Gc.major_slice 2_000 |> ignore;
    ignore (Sys.opaque_identity (List.init 500 (fun i -> i)))
  done

let () =
  let rounds =
    let test_size =
      try int_of_string (Sys.getenv "OCAML_TEST_SIZE")
      with Not_found | Failure _ -> 0
    in
    if test_size >= 3 then 400 else 150
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
