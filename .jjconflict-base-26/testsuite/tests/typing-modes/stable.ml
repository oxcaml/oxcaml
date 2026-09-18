(* TEST
 expect;
*)

(* Test that [include functor] in signatures preserves modality annotations.
   We use Stable maturity level to zap modalities to floor, since signature
   inclusion typically constrains a corresponding [include functor] in the
   structure. *)

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
