(* TEST
  include ocamlcommon;
*)

let buffer = lazy (Buffer.create 16)

let print_result input =
  let buffer = Lazy.force buffer in
  Printf.printf "'%s' -> " input;
  try
    let result = Load_path.For_testing.split_and_unescape ~buffer input in
    Printf.printf "[%s]\n" (String.concat "; " (List.map (Printf.sprintf "%S") result))
  with
  | Load_path.For_testing.Parse_error msg ->
    Printf.printf "Parse_error(%S)\n" msg
;;

let () =
  print_result "a b c";
  print_result "a\\ b c";
  print_result "a\\\\b c";
  print_result "foo\\ \\ \\ bar\\   hello";
  print_result "a\\nb";
  print_result "a\\rb";
  print_result "a\\\\b c\\ d e\\nf g\\rh";
  print_result "a  b   c";
  print_result " a b";
  print_result "a b ";
  print_result "";
  print_result "   ";
  print_result "   \\n";
  print_result "a\\xb";
  print_result "a b\\"
;;
