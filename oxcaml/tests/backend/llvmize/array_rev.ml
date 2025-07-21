let arr = Sys.opaque_identity Array_rev_data.arr

let len = Sys.opaque_identity Array_rev_data.len

let rev () =
  for i = 0 to (len / 2) - 1 do
    let temp = Array.unsafe_get arr i in
    Array.unsafe_set arr i (Array.unsafe_get arr (len - 1 - i));
    Array.unsafe_set arr (len - 1 - i) temp
  done
