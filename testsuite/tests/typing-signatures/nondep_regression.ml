(* TEST
 expect;
*)

type 'a seq = 'a list

module Make (A : sig type t end) = struct
  type t = A.t seq
end

module H = Make (struct type t end)

[%%expect{|
type 'a seq = 'a list
<<<<<<< HEAD
module Make : functor (A : sig type t end) -> sig type t = A.t seq end
module H : sig type t : value mod non_float end
||||||| 23e84b8c4d
module Make : functor (A : sig type t end) -> sig type t = A.t seq end
module H : sig type t end
=======
module Make : (A : sig type t end) -> sig type t = A.t seq end
module H : sig type t end
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
|}]
