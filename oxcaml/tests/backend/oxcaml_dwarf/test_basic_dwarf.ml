let simple_function x y = x + y

let nested_function a =
  let helper b = b * 2 in
  helper a

let rec recursive_function n =
  if n <= 1 then 1
  else n * recursive_function (n - 1)

let () =
  let result1 = simple_function 3 4 in
  let result2 = nested_function 5 in
  let result3 = recursive_function 4 in
  Printf.printf "simple_function 3 4 = %d\n" result1;
  Printf.printf "nested_function 5 = %d\n" result2;
  Printf.printf "recursive_function 4 = %d\n" result3;
  Printf.printf "DWARF test completed - this should be updated with real DWARF checks\n"