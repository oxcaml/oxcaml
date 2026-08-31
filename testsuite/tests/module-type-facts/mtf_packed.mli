module Mtf_pack_a : sig module type S = sig type t end end

module Mtf_pack_b : sig module M : Mtf_pack_a.S end
