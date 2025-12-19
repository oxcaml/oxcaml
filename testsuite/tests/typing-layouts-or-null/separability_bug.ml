(* TEST
 expect;
*)

type t = int or_null

module type S = sig
  type t : any mod separable
end

[%%expect{|
type t = int or_null
module type S = sig type t : any separable end
|}]


module type S' = S with type t = t

[%%expect{|
module type S' = sig type t = t end
|}]
