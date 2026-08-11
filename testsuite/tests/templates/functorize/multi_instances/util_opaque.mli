(* Parameters: P, and depends on Basic_opaque *)

(* By declaring type t = Basic_opaque.t, both modules share the same abstract
   type within a single bundle application, while two bundle applications each
   get fresh distinct types. *)
type t = Basic_opaque.t

val create : unit -> t
val to_string : t -> string
