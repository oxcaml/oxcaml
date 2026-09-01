module Arg : sig
  val f : (Unloaded.Record.t# -> unit) -> unit
end = struct
  let f _ = ()
end

module type S = sig
  module A : sig
    val f : (Unloaded.Record.t# -> unit) -> unit
  end

  type s
end

module Make (_ : sig end) = struct
  module A = Arg

  type s = int
end

type w = Make(Arg).s
type z = int
