let read_all ic =
  let buffer = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_channel buffer ic 4096
     done
   with End_of_file -> ());
  Buffer.contents buffer

let () =
  let filename =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else "stdin.ml"
  in
  let source = read_all stdin in
  let output =
    Web_bytecode_check.check_string ~browser:false ~filename ~source
  in
  print_string output
