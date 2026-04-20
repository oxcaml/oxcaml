(* Gc_compact_test: tests GC interaction after manual module init *)

(* This module is initialized separately, then we call test_gc
   from C code after all initialization is complete *)

let () = Printf.printf "Gc_compact_test: initializing\n%!"

(* Depend on other modules to ensure they're all initialized *)
let check_values () =
  Printf.printf "Gc_compact_test: Base.base_value = %d\n%!" Base.base_value;
  Printf.printf "Gc_compact_test: Dep_a.dep_a_value = %d\n%!" Dep_a.dep_a_value;
  Printf.printf "Gc_compact_test: Dep_b.dep_b_value = %d\n%!" Dep_b.dep_b_value;
  Printf.printf "Gc_compact_test: Diamond.combined = %d\n%!" Diamond.combined;
  Printf.printf "Gc_compact_test: Gc_test.final_value = %d\n%!" Gc_test.final_value;
  Printf.printf "Gc_compact_test: Closures.closures_value = %d\n%!" Closures.closures_value

(* Allocate data that will survive GC *)
let persistent_data = Array.init 100 (fun i -> Array.make 100 i)

(* This function will be called from C after init is complete *)
let test_gc () =
  Printf.printf "Gc_compact_test: before Gc.full_major()\n%!";
  check_values ();

  Printf.printf "Gc_compact_test: running Gc.full_major()\n%!";
  Gc.full_major ();
  Printf.printf "Gc_compact_test: Gc.full_major() completed\n%!";

  Printf.printf "Gc_compact_test: running Gc.compact()\n%!";
  Gc.compact ();
  Printf.printf "Gc_compact_test: Gc.compact() completed\n%!";

  (* Verify all data is still accessible after GC *)
  check_values ();

  (* Check our persistent data *)
  let sum = Array.fold_left (fun acc arr -> acc + Array.fold_left (+) 0 arr) 0 persistent_data in
  Printf.printf "Gc_compact_test: persistent_data sum = %d\n%!" sum;

  (* Use closures from Dep_a *)
  Printf.printf "Gc_compact_test: Dep_a.add_seed 1000 = %d\n%!" (Dep_a.add_seed 1000);

  Printf.printf "Gc_compact_test: all checks passed\n%!"

(* Register the callback so C can call it *)
let () = Callback.register "test_gc" test_gc [@@alert "-unsafe_multidomain"]
