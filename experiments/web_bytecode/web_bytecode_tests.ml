let contains_substring haystack needle =
  let haystack_length = String.length haystack in
  let needle_length = String.length needle in
  let rec loop index =
    if needle_length = 0 then true
    else if index + needle_length > haystack_length then false
    else if String.sub haystack index needle_length = needle then true
    else loop (index + 1)
  in
  loop 0

let fail message =
  prerr_endline message;
  exit 1

let expect_equal name expected actual =
  if not (String.equal expected actual) then
    fail
      (Printf.sprintf
         "%s: expected %S, got %S"
         name
         expected
         actual)

let expect_contains name needle haystack =
  if not (contains_substring haystack needle) then
    fail
      (Printf.sprintf
         "%s: expected substring %S in %S"
         name
         needle
         haystack)

let () =
  let check_ok =
    Web_bytecode_native.check_string
      ~filename:"check_ok.ml"
      ~source:"let x = 1\n"
  in
  expect_equal "check_ok" "" check_ok;
  let check_syntax_error =
    Web_bytecode_native.check_string
      ~filename:"check_syntax_error.ml"
      ~source:"let =\n"
  in
  expect_contains "check_syntax_error" "Syntax error" check_syntax_error;
  let check_type_error =
    Web_bytecode_native.check_string
      ~filename:"check_type_error.ml"
      ~source:"let x : int = \"nope\"\n"
  in
  expect_contains "check_type_error" "Error:" check_type_error;
  let run_ok =
    Web_bytecode_native.run_string
      ~filename:"run_ok.ml"
      ~source:"print_endline \"hi\";;\n"
  in
  expect_equal "run_ok" "hi\n" run_ok;
  let run_error =
    Web_bytecode_native.run_string
      ~filename:"run_error.ml"
      ~source:"raise (Failure \"boom\");;\n"
  in
  expect_contains "run_error" "Failure \"boom\"" run_error;
  let run_reset_1 =
    Web_bytecode_native.run_string
      ~filename:"run_reset_1.ml"
      ~source:"let x = 1;;\nprint_endline \"first\";;\n"
  in
  expect_equal "run_reset_1" "first\n" run_reset_1;
  let run_reset_2 =
    Web_bytecode_native.run_string
      ~filename:"run_reset_2.ml"
      ~source:"x;;\n"
  in
  expect_contains "run_reset_2" "Unbound value x" run_reset_2;
  let check_parallel =
    Web_bytecode_native.check_string
      ~filename:"check_parallel.ml"
      ~source:
        "open Core\n\
         let scheduler = Parallel.Scheduler.Sequential.create ()\n\
         let run () =\n\
           Parallel.Scheduler.Sequential.parallel scheduler ~f:(fun parallel ->\n\
             let #(a, b) = Parallel.fork_join2 parallel (fun _ -> 21) (fun _ -> 21) in\n\
             a + b)\n"
  in
  expect_equal "check_parallel" "" check_parallel;
  let run_parallel =
    Web_bytecode_native.run_string
      ~filename:"run_parallel.ml"
      ~source:
        "open Core\n\
         let scheduler = Parallel.Scheduler.Sequential.create ()\n\
         let () =\n\
           Parallel.Scheduler.Sequential.parallel scheduler ~f:(fun parallel ->\n\
             let #(a, b) = Parallel.fork_join2 parallel (fun _ -> 21) (fun _ -> 21) in\n\
             Stdlib.print_endline (Int.to_string (a + b)))\n"
  in
  expect_equal "run_parallel" "42\n" run_parallel;
  let run_parallel_recursive =
    Web_bytecode_native.run_string
      ~filename:"run_parallel_recursive.ml"
      ~source:
        "open Core\n\
         let rec fib_par parallel n =\n\
           match n with\n\
           | 0 | 1 -> 1\n\
           | n ->\n\
             let #(a, b) =\n\
               Parallel.fork_join2\n\
                 parallel\n\
                 (fun parallel -> fib_par parallel (n - 1))\n\
                 (fun parallel -> fib_par parallel (n - 2))\n\
             in\n\
             a + b\n\
         let () =\n\
           let scheduler = Parallel.Scheduler.Sequential.create () in\n\
           Parallel.Scheduler.Sequential.parallel scheduler ~f:(fun parallel ->\n\
             Stdlib.print_endline (Int.to_string (fib_par parallel 10)))\n"
  in
  expect_equal "run_parallel_recursive" "89\n" run_parallel_recursive;
  print_endline "web_bytecode tests passed"
