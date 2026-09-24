(* Host-side unit whose fields the plugins read: a static closure, dynamic
   fields and a slot for the plugins' reports. *)

module type S = sig
  val f : int -> int
  val n : int
  val r : int ref
  val report : string list ref
end

let f x = x + 100
let n = Sys.opaque_identity 5
let r = ref 1
let report : string list ref = ref []
