(* Compiled with -bin-annot-cms only. *)
module type S = sig type t end
module M : S = struct type t = int end
