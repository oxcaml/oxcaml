module Flat (T : sig type t : void + void + value end) = struct
  external box : T.t -> T.t box = "%box"
  external unbox : T.t box -> T.t = "%unbox"
  let[@inline never] pack v = box v
  let[@inline never] unpack v = unbox v
end

module Grouped (T : sig type t : void + (void + value) end) = struct
  external box : T.t -> T.t box = "%box"
  external unbox : T.t box -> T.t = "%unbox"
  let[@inline never] pack v = box v
  let[@inline never] unpack v = unbox v
end

module Product (T : sig type t : void + (void & (void + value)) end) = struct
  external box : T.t -> T.t box = "%box"
  external unbox : T.t box -> T.t = "%unbox"
  let[@inline never] pack v = box v
  let[@inline never] unpack v = unbox v
end

module Deep (T : sig type t : void + void + (void + value) end) = struct
  external box : T.t -> T.t box = "%box"
  external unbox : T.t box -> T.t = "%unbox"
  let[@inline never] pack v = box v
  let[@inline never] unpack v = unbox v
end

module Sibling (T : sig type t : void + void + value + value end) = struct
  external box : T.t -> T.t box = "%box"
  external unbox : T.t box -> T.t = "%unbox"
  let[@inline never] pack v = box v
  let[@inline never] unpack v = unbox v
end

module Both (T : sig type t : void + void + value + (void + value) end) =
struct
  external box : T.t -> T.t box = "%box"
  external unbox : T.t box -> T.t = "%unbox"
  let[@inline never] pack v = box v
  let[@inline never] unpack v = unbox v
end

module Shifted (T : sig
  type t : value + void + void + value + value + void
           + (value & bits64) + (bits64 & value)
end) = struct
  external box : T.t -> T.t box = "%box"
  external unbox : T.t box -> T.t = "%unbox"
  let[@inline never] pack v = box v
  let[@inline never] unpack v = unbox v
end

module Mixed_sources = struct
  type inner = Off | On of string
  module Sibling = struct
    type t = Empty | Direct of inner# | Record of { mutable number : int }
  end
  module Both = struct
    type t = Empty | Direct of inner# | Record of { mutable inner : inner# }
  end
  module Shifted = struct
    type choices = No | Left of string | Right of string
    type t = Head of int | First | Direct of choices# | Last
           | Record of { mutable name : string; count : int64_u }
           | Pair of int64_u * string
    let[@inline never] record name = Record { name; count = #9L }
    let[@inline never] reconstruct = function Record r -> Record r | v -> v
    let[@inline never] rename v name = match v with
      | Record r -> r.name <- name | _ -> assert false
  end
end
