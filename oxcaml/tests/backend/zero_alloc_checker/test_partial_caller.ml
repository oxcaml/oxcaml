[@@@zero_alloc check]

(* fails to compile: *)
let[@zero_alloc] g x =
  (* In another file, [./test_partial_caller_lib.ml], we define
     [Test_partial_caller_lib.f x y = x + y]. Note that [f x] *will* allocate.
     However, [./test_partial_caller_lib.ml] has [@@@zero_alloc check none],
     which inserts [zero_alloc] certificates *without checking anything*, so
     the line below can see only that the certificate exists, not that it's been
     forged. However, since we perform a redundant check for allocations at call
     sites, we catch this source of unsoundness before it runs. *)
  Test_partial_caller_lib.f x
