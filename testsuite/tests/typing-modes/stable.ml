(* TEST
 expect;
*)
module type Empty = sig end

module type F = functor (_ : Empty) -> sig val foo : unit -> unit @@ portable end
module type S = sig
  include functor F
end
[%%expect{|
module type Empty = sig end
module type F = Empty -> sig val foo : unit -> unit @@ portable end
module type S = sig val foo : unit -> unit @@ portable end
|}]
