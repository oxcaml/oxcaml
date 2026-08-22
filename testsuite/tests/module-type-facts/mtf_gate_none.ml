(* Compiled with neither -bin-annot nor -bin-annot-cms. *)
module type S = sig type t end
module M : S = struct type t = int end
