let poly_ id x = x

let poly_ pair x y = #(x, y)

let calls = ref 0

let poly_ counted_id x =
  incr calls;
  x

module Stamper = functor[@inline never] (X : sig @@ dynamic
  val offset : int
  val counter : int ref
end @ static) -> struct
  let () = incr calls
  let initial = !X.counter
  let[@inline never] poly_ with_stamp x =
    incr calls;
    incr X.counter;
    #(X.offset + !X.counter, x)
end
