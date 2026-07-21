module Arg : sig
  val f : (Unloaded.Record.t# -> unit) -> unit
end

module type S = sig
  module A : sig
    val f : (Unloaded.Record.t# -> unit) -> unit
  end

  type s
end

module Make (_ : sig end) : S with module A = Arg

type w = Make(Arg).s
type z
