let poly_ id x = x

let poly_ pair x y = #(x, y)

let calls = ref 0

let poly_ counted_id x =
  incr calls;
  x
