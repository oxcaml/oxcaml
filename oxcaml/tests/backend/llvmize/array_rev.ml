module D = Array_rev_data

let rev () =
  for i = 0 to (D.len / 2) - 1 do
    let temp = Array.unsafe_get D.arr i in
    Array.unsafe_set D.arr i (Array.unsafe_get D.arr (D.len - 1 - i));
    Array.unsafe_set D.arr (D.len - 1 - i) temp
  done
