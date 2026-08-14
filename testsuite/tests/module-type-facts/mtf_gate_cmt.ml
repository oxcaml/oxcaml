(* Compiled with -bin-annot only. *)
module type S = sig type t end
module M : S = struct type t = int end
