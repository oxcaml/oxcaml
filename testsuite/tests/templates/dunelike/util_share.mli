(* Parameters: P, and depends on Basic_share *)

(* By declaring type t = Basic_share.t, both modules share the same abstract
   type within a single bundle application, while two bundle applications each
   get fresh distinct types. *)
type t = Basic_share.t

val create : unit -> t
val to_string : t -> string
