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
