(* Generator for large OCaml modules to test cross-partition calls.

   Usage: Run during build via dune

   This generates modules that compile to ~3.6MB object files each, ensuring
   they end up in different partitions when using -dissector-partition-size
   0.002 (2MB). The compiled size is much larger than source due to array
   literal expansion. *)

(* Parameters tuned to produce ~3.6MB object files per module. This ensures
   modules end up in separate partitions even with a 2MB partition threshold,
   providing headroom for stdlib growth. Note: array_size is limited to avoid
   stack overflow during compilation. *)
let num_functions = 10000

let array_size = 13000

let output_file name content =
  let oc = open_out name in
  output_string oc content;
  close_out oc

let generate_array_literal prefix size =
  let buf = Buffer.create (size * 10) in
  Buffer.add_string buf "let arr = [|\n";
  for i = 0 to size - 1 do
    if i > 0 then Buffer.add_string buf ";";
    if i mod 20 = 0 then Buffer.add_string buf "\n  ";
    Buffer.add_string buf (string_of_int (i + prefix))
  done;
  Buffer.add_string buf "\n|]\n\n";
  Buffer.contents buf

let generate_functions prefix num =
  let buf = Buffer.create (num * 100) in
  for i = 0 to num - 1 do
    Printf.bprintf buf "let f%05d x = x + %d\n" i (prefix + i)
  done;
  Buffer.add_char buf '\n';
  (* Generate call_all by filling an array with all function results, then
     summing. This avoids stack overflow from deeply nested expressions. *)
  Printf.bprintf buf "let call_all x =\n";
  Printf.bprintf buf "  let results = [|\n";
  for i = 0 to num - 1 do
    if i > 0 then Buffer.add_string buf ";";
    if i mod 10 = 0 then Buffer.add_string buf "\n    ";
    Printf.bprintf buf "f%05d x" i
  done;
  Buffer.add_string buf "\n  |] in\n";
  Buffer.add_string buf "  Array.fold_left (+) 0 results\n\n";
  Buffer.contents buf

let generate_module_a () =
  let buf = Buffer.create 100000 in
  Buffer.add_string buf "(* Generated module A - do not edit *)\n\n";
  Buffer.add_string buf (generate_array_literal 0 array_size);
  Buffer.add_string buf "let counter = ref 0\n";
  Buffer.add_string buf "let increment () = incr counter; !counter\n";
  Buffer.add_string buf "let get_counter () = !counter\n";
  Buffer.add_string buf "let get_magic () = 42\n";
  (* Static constant that other modules will reference directly for IGOT
     testing *)
  Buffer.add_string buf "let static_data = [| 100; 200; 300; 400; 500 |]\n\n";
  Buffer.add_string buf (generate_functions 1 num_functions);
  Buffer.contents buf

let generate_module_b () =
  let buf = Buffer.create 100000 in
  Buffer.add_string buf "(* Generated module B - do not edit *)\n";
  Buffer.add_string buf "(* Depends on Gen_module_a *)\n\n";
  Buffer.add_string buf (generate_array_literal 200000 array_size);
  Buffer.add_string buf "let counter = ref 0\n";
  Buffer.add_string buf "let increment () = incr counter; !counter\n";
  Buffer.add_string buf "let get_counter () = !counter\n";
  (* Static constant that other modules will reference directly for IGOT
     testing *)
  Buffer.add_string buf "let static_data = [| 600; 700; 800; 900; 1000 |]\n\n";
  Buffer.add_string buf "(* Cross-partition calls to A *)\n";
  Buffer.add_string buf "let call_a_increment () = Gen_module_a.increment ()\n";
  Buffer.add_string buf "let call_a_magic () = Gen_module_a.get_magic ()\n";
  Buffer.add_string buf "let call_a_functions x = Gen_module_a.call_all x\n";
  Buffer.add_string buf "let get_a_arr_element i = Gen_module_a.arr.(i)\n\n";
  (* Direct mutable data access across partitions *)
  Buffer.add_string buf "(* Direct mutable data access across partitions *)\n";
  Buffer.add_string buf
    "let read_a_counter_direct () = !(Gen_module_a.counter)\n";
  Buffer.add_string buf
    "let write_a_counter_direct v = Gen_module_a.counter := v\n";
  (* Direct reference to static data - forces IGOT entry *)
  Buffer.add_string buf
    "let get_a_static_data () = Gen_module_a.static_data\n\n";
  (* Closures capturing cross-partition references *)
  Buffer.add_string buf "(* Closures capturing cross-partition references *)\n";
  Buffer.add_string buf
    "let make_a_incrementer () = fun () -> Gen_module_a.increment ()\n";
  Buffer.add_string buf
    "let make_a_arr_reader i = fun () -> Gen_module_a.arr.(i)\n";
  Buffer.add_string buf "let captured_a_increment = Gen_module_a.increment\n\n";
  Buffer.add_string buf (generate_functions 10001 num_functions);
  Buffer.contents buf

let generate_module_c () =
  let buf = Buffer.create 100000 in
  Buffer.add_string buf "(* Generated module C - do not edit *)\n";
  Buffer.add_string buf "(* Depends on Gen_module_a and Gen_module_b *)\n\n";
  Buffer.add_string buf (generate_array_literal 400000 array_size);
  Buffer.add_string buf "let counter = ref 0\n";
  Buffer.add_string buf "let increment () = incr counter; !counter\n";
  Buffer.add_string buf "let get_counter () = !counter\n\n";
  Buffer.add_string buf "(* Cross-partition calls to A and B *)\n";
  Buffer.add_string buf "let call_a_increment () = Gen_module_a.increment ()\n";
  Buffer.add_string buf "let call_b_increment () = Gen_module_b.increment ()\n";
  Buffer.add_string buf "let call_a_magic () = Gen_module_a.get_magic ()\n";
  Buffer.add_string buf
    "let call_both_functions x = Gen_module_a.call_all x + \
     Gen_module_b.call_all x\n";
  Buffer.add_string buf "let get_a_arr_element i = Gen_module_a.arr.(i)\n";
  Buffer.add_string buf "let get_b_arr_element i = Gen_module_b.arr.(i)\n\n";
  (* Direct mutable data access across partitions *)
  Buffer.add_string buf "(* Direct mutable data access across partitions *)\n";
  Buffer.add_string buf
    "let read_a_counter_direct () = !(Gen_module_a.counter)\n";
  Buffer.add_string buf
    "let read_b_counter_direct () = !(Gen_module_b.counter)\n";
  Buffer.add_string buf
    "let write_a_counter_direct v = Gen_module_a.counter := v\n";
  Buffer.add_string buf
    "let write_b_counter_direct v = Gen_module_b.counter := v\n";
  (* Direct references to static data - forces IGOT entries *)
  Buffer.add_string buf "let get_a_static_data () = Gen_module_a.static_data\n";
  Buffer.add_string buf
    "let get_b_static_data () = Gen_module_b.static_data\n\n";
  (* Closures capturing cross-partition references *)
  Buffer.add_string buf "(* Closures capturing cross-partition references *)\n";
  Buffer.add_string buf
    "let make_combined_incrementer () = fun () -> Gen_module_a.increment () + \
     Gen_module_b.increment ()\n";
  Buffer.add_string buf
    "let make_multi_arr_reader i j = fun () -> Gen_module_a.arr.(i) + \
     Gen_module_b.arr.(j)\n";
  Buffer.add_string buf
    "let captured_b_via_a = Gen_module_b.captured_a_increment\n\n";
  Buffer.add_string buf (generate_functions 20001 num_functions);
  Buffer.contents buf

let generate_main () =
  {|(* Generated main - do not edit *)

let () =
  print_endline "=== Cross-partition test ===";
  print_newline ();

  (* Test 1: Direct calls to module A *)
  print_endline "Test 1: Direct calls to Gen_module_a";
  let a1 = Gen_module_a.increment () in
  let a2 = Gen_module_a.increment () in
  Printf.printf "  Counter: %d, %d\n" a1 a2;
  let magic = Gen_module_a.get_magic () in
  Printf.printf "  Magic: %d\n" magic;
  let elem = Gen_module_a.arr.(5) in
  Printf.printf "  arr[5]: %d\n" elem;
  let f_result = Gen_module_a.f00010 100 in
  Printf.printf "  f00010(100): %d\n" f_result;
  print_newline ();

  (* Test 2: Calls through module B to A *)
  print_endline "Test 2: Calls through Gen_module_b";
  let b1 = Gen_module_b.increment () in
  Printf.printf "  B.increment: %d\n" b1;
  let a_from_b = Gen_module_b.call_a_increment () in
  Printf.printf "  B.call_a_increment: %d\n" a_from_b;
  let a_magic = Gen_module_b.call_a_magic () in
  Printf.printf "  B.call_a_magic: %d\n" a_magic;
  let a_elem = Gen_module_b.get_a_arr_element 10 in
  Printf.printf "  B.get_a_arr_element(10): %d\n" a_elem;
  let b_elem = Gen_module_b.arr.(15) in
  Printf.printf "  B.arr[15]: %d\n" b_elem;
  print_newline ();

  (* Test 3: Calls through module C to A and B *)
  print_endline "Test 3: Calls through Gen_module_c";
  let c1 = Gen_module_c.increment () in
  Printf.printf "  C.increment: %d\n" c1;
  let a_from_c = Gen_module_c.call_a_increment () in
  Printf.printf "  C.call_a_increment: %d\n" a_from_c;
  let b_from_c = Gen_module_c.call_b_increment () in
  Printf.printf "  C.call_b_increment: %d\n" b_from_c;
  let c_magic = Gen_module_c.call_a_magic () in
  Printf.printf "  C.call_a_magic: %d\n" c_magic;
  let a_elem_c = Gen_module_c.get_a_arr_element 20 in
  Printf.printf "  C.get_a_arr_element(20): %d\n" a_elem_c;
  let b_elem_c = Gen_module_c.get_b_arr_element 25 in
  Printf.printf "  C.get_b_arr_element(25): %d\n" b_elem_c;
  let c_elem = Gen_module_c.arr.(30) in
  Printf.printf "  C.arr[30]: %d\n" c_elem;
  print_newline ();

  (* Test 4: call_all functions *)
  print_endline "Test 4: call_all functions";
  let a_all = Gen_module_a.call_all 1 in
  Printf.printf "  A.call_all(1): %d\n" a_all;
  let b_all = Gen_module_b.call_all 1 in
  Printf.printf "  B.call_all(1): %d\n" b_all;
  let c_all = Gen_module_c.call_all 1 in
  Printf.printf "  C.call_all(1): %d\n" c_all;
  let cross_b = Gen_module_b.call_a_functions 1 in
  Printf.printf "  B.call_a_functions(1): %d\n" cross_b;
  let cross_c = Gen_module_c.call_both_functions 1 in
  Printf.printf "  C.call_both_functions(1): %d\n" cross_c;
  print_newline ();

  (* Test 5: Direct mutable data access across partitions *)
  print_endline "Test 5: Direct mutable data access";
  Printf.printf "  B.read_a_counter_direct: %d\n"
    (Gen_module_b.read_a_counter_direct ());
  Gen_module_b.write_a_counter_direct 100;
  Printf.printf "  After B.write_a_counter_direct(100): %d\n"
    (Gen_module_a.get_counter ());
  Printf.printf "  C.read_a_counter_direct: %d\n"
    (Gen_module_c.read_a_counter_direct ());
  Printf.printf "  C.read_b_counter_direct: %d\n"
    (Gen_module_c.read_b_counter_direct ());
  Gen_module_c.write_a_counter_direct 200;
  Gen_module_c.write_b_counter_direct 50;
  Printf.printf "  After C writes - A.counter: %d, B.counter: %d\n"
    (Gen_module_a.get_counter ()) (Gen_module_b.get_counter ());
  let a_static = Gen_module_b.get_a_static_data () in
  Printf.printf "  B.get_a_static_data()[0]: %d\n" a_static.(0);
  let a_static_c = Gen_module_c.get_a_static_data () in
  let b_static_c = Gen_module_c.get_b_static_data () in
  Printf.printf "  C.get_a_static_data()[1]: %d\n" a_static_c.(1);
  Printf.printf "  C.get_b_static_data()[2]: %d\n" b_static_c.(2);
  print_newline ();

  (* Test 6: Closures capturing cross-partition references *)
  print_endline "Test 6: Closures with cross-partition refs";
  let a_inc = Gen_module_b.make_a_incrementer () in
  Printf.printf "  B.make_a_incrementer()(): %d\n" (a_inc ());
  Printf.printf "  B.make_a_incrementer()() again: %d\n" (a_inc ());
  let arr_reader = Gen_module_b.make_a_arr_reader 7 in
  Printf.printf "  B.make_a_arr_reader(7)(): %d\n" (arr_reader ());
  Printf.printf "  B.captured_a_increment(): %d\n"
    (Gen_module_b.captured_a_increment ());
  let combined_inc = Gen_module_c.make_combined_incrementer () in
  Printf.printf "  C.make_combined_incrementer()(): %d\n" (combined_inc ());
  let multi_reader = Gen_module_c.make_multi_arr_reader 3 5 in
  Printf.printf "  C.make_multi_arr_reader(3,5)(): %d\n" (multi_reader ());
  Printf.printf "  C.captured_b_via_a(): %d\n"
    (Gen_module_c.captured_b_via_a ());
  print_newline ();

  (* Test 7: Final counter state *)
  print_endline "Test 7: Final counter state";
  Printf.printf "  A.counter: %d\n" (Gen_module_a.get_counter ());
  Printf.printf "  B.counter: %d\n" (Gen_module_b.get_counter ());
  Printf.printf "  C.counter: %d\n" (Gen_module_c.get_counter ());
  print_newline ();

  print_endline "=== All tests passed! ==="
|}

let generate_mylib_a () =
  let buf = Buffer.create 100000 in
  Buffer.add_string buf "(* Generated mylib_a - do not edit *)\n\n";
  Buffer.add_string buf (generate_array_literal 0 array_size);
  Buffer.add_string buf "let counter = ref 0\n";
  Buffer.add_string buf "let increment () = incr counter; !counter\n";
  Buffer.add_string buf "let sum_data () = Array.fold_left (+) 0 arr\n";
  Buffer.add_string buf "let double x = x * 2\n";
  Buffer.add_string buf "let triple x = x * 3\n";
  Buffer.add_string buf "let square x = x * x\n\n";
  Buffer.add_string buf (generate_functions 1 num_functions);
  Buffer.contents buf

let generate_mylib_b () =
  let buf = Buffer.create 100000 in
  Buffer.add_string buf "(* Generated mylib_b - do not edit *)\n";
  Buffer.add_string buf "(* Depends on Mylib_a *)\n\n";
  Buffer.add_string buf (generate_array_literal 200000 array_size);
  Buffer.add_string buf "let call_increment () = Mylib_a.increment ()\n";
  Buffer.add_string buf "let call_double x = Mylib_a.double x\n";
  Buffer.add_string buf
    "let combined x = Mylib_a.double x + Mylib_a.triple x - Mylib_a.square 2\n";
  Buffer.add_string buf "let get_data_element i = Mylib_a.arr.(i)\n\n";
  Buffer.add_string buf (generate_functions 10001 num_functions);
  Buffer.contents buf

let generate_cmxa_main () =
  {|(* Generated cmxa main - do not edit *)

let () =
  print_endline "=== CMXA archive test ===";
  print_newline ();

  print_endline "Test 1: Direct calls to Mylib_a";
  let c1 = Mylib_a.increment () in
  let c2 = Mylib_a.increment () in
  Printf.printf "  Counter: %d, %d\n" c1 c2;
  Printf.printf "  double(5): %d\n" (Mylib_a.double 5);
  Printf.printf "  triple(5): %d\n" (Mylib_a.triple 5);
  Printf.printf "  square(5): %d\n" (Mylib_a.square 5);
  print_newline ();

  print_endline "Test 2: Calls through Mylib_b";
  let c3 = Mylib_b.call_increment () in
  Printf.printf "  call_increment: %d\n" c3;
  Printf.printf "  call_double(7): %d\n" (Mylib_b.call_double 7);
  Printf.printf "  combined(10): %d\n" (Mylib_b.combined 10);
  Printf.printf "  get_data_element(2): %d\n" (Mylib_b.get_data_element 2);
  print_newline ();

  print_endline "=== All tests passed! ==="
|}

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | ["gen_module_a"] -> output_file "gen_module_a.ml" (generate_module_a ())
  | ["gen_module_b"] -> output_file "gen_module_b.ml" (generate_module_b ())
  | ["gen_module_c"] -> output_file "gen_module_c.ml" (generate_module_c ())
  | ["gen_main"] -> output_file "gen_main.ml" (generate_main ())
  | ["mylib_a"] -> output_file "mylib_a.ml" (generate_mylib_a ())
  | ["mylib_b"] -> output_file "mylib_b.ml" (generate_mylib_b ())
  | ["cmxa_main"] -> output_file "cmxa_main.ml" (generate_cmxa_main ())
  | _ ->
    Printf.eprintf
      "Usage: %s \
       <gen_module_a|gen_module_b|gen_module_c|gen_main|mylib_a|mylib_b|cmxa_main>\n"
      Sys.argv.(0);
    exit 1
