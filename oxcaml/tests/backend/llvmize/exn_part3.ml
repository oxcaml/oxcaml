open Exn_part1
open Exn_part2

let round_trip () =
  try
    raise_exn1_catch_exn2_from_llvm ();
    -1
  with Exn1 n -> n

let () =
  print_endline "catch_exn1_from_llvm";
  catch_exn1_from_llvm ();
  print_endline "catch_exn1_nested_from_llvm";
  catch_exn1_nested_from_llvm ();
  Format.printf "complicated (raise_1 100) %d\n" (complicated (raise_1 100));
  Format.printf "complicated (raise_2): %d\n" (complicated raise_2);
  Format.printf "complicated (raise_3): %d\n" (complicated raise_3);
  Format.printf "round_trip: %d\n" (round_trip ());
  print_endline "raise_in_loop";
  raise_in_loop ();
  print_endline "catch_wildcard";
  catch_wildcard ()
