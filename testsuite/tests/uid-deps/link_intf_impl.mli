type t (* 0 *)

val x (* 1 *) : t

module type S (* 4 *) = sig
  val y (* 2 *) : t
end

module M (* 5 *) : S

module type Initial (* 10 *) = sig
  module type Nested (* 8 *) = sig
    type t (* 6 *)
  end
end

module FMT (* 18 *) (X (* 15 *) : sig
  module type MT (* 13 *) = sig val x (* 11 *) : int end
end) : sig end
