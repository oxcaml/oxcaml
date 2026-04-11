let () =
  let output =
    Web_bytecode_check.check_string
      ~browser:true
      ~filename:"demo.ml"
      ~source:"let x : string = 1\n"
  in
  print_string output
