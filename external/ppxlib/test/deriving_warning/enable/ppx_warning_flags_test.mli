type t [@@deriving_inline zero_do_warn, one_no_warn, two_do_warn]

module Zero : sig
  type t
end

val zero : Zero.t

include sig
  [@@@ocaml.warning "-32-60"]

  module One : sig
    type 'a t
  end

  val one : Zero.t One.t
end
[@@ocaml.doc "@inline"]

module Two : sig
  type ('a, 'b) t
end

val two : (Zero.t, Zero.t One.t) Two.t

[@@@end]

type s = int [@@deriving_inline alias_warn]

val unit_one : unit

include sig
  [@@@ocaml.warning "-32"]

  val unit_two : unit
end
[@@ocaml.doc "@inline"]

[@@@end]
