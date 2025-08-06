let add x y = x + y
let multiply x y = x * y

let compute_result () =
  let a = add 10 20 in
  let b = multiply 3 4 in
  add a b

let () =
  let result = compute_result () in
  Printf.printf "Computed result: %d\n" result;
  Printf.printf "This demonstrates DWARF info for nested function calls\n"