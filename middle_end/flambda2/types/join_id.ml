include Numeric_types.Int

let create =
  let cnt = ref 0 in
  fun () ->
    incr cnt;
    !cnt
