(* Fails to compile, so that its [.cmt] holds a partial implementation. *)
module type S = sig type t end
module M : S = struct type u = int end
