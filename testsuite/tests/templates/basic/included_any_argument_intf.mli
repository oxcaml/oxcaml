(* Like [included_argument_intf.mli], but [t] is still [any] after the
   [include], so [x] isn't representable and this can't be used as an argument
   for [Unrepresentable_param]. *)
module type S = sig
  type t : any
  val x : t
end

include S

val y : int
