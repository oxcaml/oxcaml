[@@@zero_alloc check]

(* fails to compile: *)
let[@zero_alloc] g x =
  (* In another file, `Test_partial_caller_lib.f x y = x + y`.
     Note that this *does* allocate a partial closure on the heap,
     but the other file is marked [@@@zero_alloc check none], so
     the checker never runs, and this is erroneously accepted. *)
  Test_partial_caller_lib.f x
