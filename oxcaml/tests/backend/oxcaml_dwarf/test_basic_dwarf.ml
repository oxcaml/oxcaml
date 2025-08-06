let[@inline never] [@local never] f (x : string) : string = x

let () =
  let result = f "hello world!" in
  print_endline result
