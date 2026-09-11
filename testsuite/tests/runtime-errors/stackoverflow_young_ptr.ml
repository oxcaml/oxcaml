(* TEST
 no-stack-checks;
 native;
*)

(* With guard-page stacks, the Stack_overflow raise must not lose the
   allocations made since young_ptr was last synced to Caml_state: a
   stale reload after the guard-page fault resurrects live minor-heap
   blocks as free space. Detect that as Gc.minor_words accounting for
   fewer words than the completed frames allocated. The recursion makes
   no C calls (which would sync young_ptr): [count] holds an immediate,
   so updating it stays a plain inline store. *)

let count = ref 0

let rec go n =
  ignore (Sys.opaque_identity (ref n));
  incr count;
  1 + go (n + 1)

let () =
  let before = int_of_float (Gc.minor_words ()) in
  (try Sys.with_async_exns (fun () -> ignore (go 0))
   with Stack_overflow -> ());
  let after = int_of_float (Gc.minor_words ()) in
  let allocated = after - before in
  (* Each completed frame allocated a two-word block before bumping
     [count]. *)
  if allocated >= !count * 2
  then print_endline "ok"
  else
    Printf.printf "minor_words went backwards: %d frames, %d words\n" !count
      allocated
