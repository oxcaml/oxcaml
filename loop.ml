let foo (arr : int array) =
  let acc = ref 0 in
  for i = 0 to Array.length arr - 2 do
    acc := !acc + arr.(i) * arr.(i + 1)
  done;
  !acc

let foo2 (arr : int array) =
  let rec loop i ~acc =
    if i >= Array.length arr - 1 then acc
    else loop (i + 1) ~acc:(acc + arr.(i) * arr.(i + 1))
  in
  loop 0 ~acc:0
