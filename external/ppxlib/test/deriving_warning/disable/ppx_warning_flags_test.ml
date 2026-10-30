type t = int [@@deriving_inline zero_do_warn, one_no_warn, two_do_warn]

let _ = fun (_ : t) -> ()

include struct
  [@@@ocaml.warning "-60"]

  module Zero = struct
    type t = T0
  end

  let zero = Zero.T0
  let _ = zero
end [@@ocaml.doc "@inline"]

include struct
  [@@@ocaml.warning "-60"]

  module One = struct
    type 'a t = T1 of 'a
  end

  let one = One.T1 zero
  let _ = one
end [@@ocaml.doc "@inline"]

include struct
  [@@@ocaml.warning "-60"]

  module Two = struct
    type ('a, 'b) t = T2 of 'a * 'b
  end

  let two = Two.T2 (zero, one)
  let _ = two
end [@@ocaml.doc "@inline"]

[@@@end]

type s = int [@@deriving_inline alias_warn]

let _ = fun (_ : s) -> ()
let unit_one = ()
let _ = unit_one
let unit_two = unit_one
let _ = unit_two

[@@@end]
