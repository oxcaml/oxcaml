let poly_ id x = x

let poly_ pair x y = #(x, y)

let calls = ref 0

let poly_ counted_id x =
  incr calls;
  x

module Capture = functor[@inline never] (X : sig @@ dynamic
  val offset : int
  val state : int ref
end @ static) -> struct
  let () = incr calls; incr X.state
  let initial = !X.state
  let[@inline never] poly_ capture x =
    incr X.state;
    #(X.offset + initial + !X.state, x)
end
