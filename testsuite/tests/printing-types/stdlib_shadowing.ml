(* TEST
 expect;
*)

(* A path of [Stdlib] is printed without its prefix only if the name is not
   shadowed in the namespace of the path. *)

module List = struct end
[%%expect {|
module List : sig end
|}]

let f (x : int Stdlib.List.t) = x
[%%expect {|
val f : int List.t -> int List.t = <fun>
|}]

module Unshadowed = struct
  let f (x : int Stdlib.Seq.t) = x
end
[%%expect {|
module Unshadowed : sig val f : int Seq.t -> int Seq.t end
|}]
