type t = { fixed : unit }
type cell = { mutable contents : t }

module Foo @ stateful = struct
  let f (x : t) = x

  (* Unexported state keeps [Foo] itself genuinely stateful. *)
  let cache = { contents = { fixed = () } }
  let _set x = cache.contents <- x
end
