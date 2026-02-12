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
<<<<<<< oxcaml
module Make : functor (A : sig type t end) -> sig type t = A.t seq end
module H : sig type t : value mod non_float end
||||||| upstream-base
module Make : functor (A : sig type t end) -> sig type t = A.t seq end
module H : sig type t end
=======
module Make : (A : sig type t end) -> sig type t = A.t seq end
module H : sig type t end
>>>>>>> upstream-incoming
|}]
