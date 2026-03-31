(* Gc_test: tests GC interaction with manual module init *)
let () = Printf.printf "Gc_test: initializing\n%!"

(* Allocate some data *)
let data = Array.init 1000 (fun i -> string_of_int i)

let () = Printf.printf "Gc_test: allocated array of %d strings\n%!" (Array.length data)

(* Run minor GC during initialization *)
let () =
  Printf.printf "Gc_test: running Gc.minor() during init\n%!";
  Gc.minor ();
  Printf.printf "Gc_test: Gc.minor() completed\n%!"

(* Verify data is still accessible after minor GC *)
let sum_lengths_1 = Array.fold_left (fun acc s -> acc + String.length s) 0 data
let () = Printf.printf "Gc_test: sum of string lengths after minor = %d\n%!" sum_lengths_1

(* Run full major GC during initialization *)
let () =
  Printf.printf "Gc_test: running Gc.full_major() during init\n%!";
  Gc.full_major ();
  Printf.printf "Gc_test: Gc.full_major() completed\n%!"

(* Verify data is still accessible after full major GC *)
let sum_lengths_2 = Array.fold_left (fun acc s -> acc + String.length s) 0 data
let () = Printf.printf "Gc_test: sum of string lengths after full_major = %d\n%!" sum_lengths_2

(* Run compact during initialization *)
let () =
  Printf.printf "Gc_test: running Gc.compact() during init\n%!";
  Gc.compact ();
  Printf.printf "Gc_test: Gc.compact() completed\n%!"

(* Verify data is still accessible after compact *)
let sum_lengths = Array.fold_left (fun acc s -> acc + String.length s) 0 data
let () = Printf.printf "Gc_test: sum of string lengths after compact = %d\n%!" sum_lengths

(* Also verify values from previously initialized modules survived GC *)
let () =
  Printf.printf "Gc_test: Diamond.combined after GC = %d\n%!" Diamond.combined;
  Printf.printf "Gc_test: Dep_a.add_seed 100 after GC = %d\n%!" (Dep_a.add_seed 100)

(* Use Diamond to ensure proper dependency *)
let final_value = Diamond.combined + sum_lengths

let () = Printf.printf "Gc_test: final_value = %d\n%!" final_value
