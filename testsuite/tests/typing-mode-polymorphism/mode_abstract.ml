(* TEST
 flags = "-extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

type ghost

module type Nonsense = sig
  type m
end

module type S = sig
  type t
  type m

  val m : (module Nonsense with type m = m)

  val alloc : (module Nonsense with type m = ghost @ 'm -> unit) -> unit -> t @ [> 'm]
end
[%%expect{|
type ghost
module type Nonsense = sig type m end
module type S =
  sig
    type t
    type m
    val m : (module Nonsense with type m = m)
    val alloc :
      (module Nonsense with type m = ghost @ [< 'm] -> unit) ->
      unit -> t @ [> 'm]
  end
|}]

module type Stack =
  S with type t = string and type m = ghost @ local -> unit

[%%expect{|
module type Stack =
  sig
    type t = string
    type m = ghost @ local -> unit
    val m : (module Nonsense with type m = m)
    val alloc :
      (module Nonsense with type m = ghost @ [< 'm] -> unit) ->
      unit -> t @ [> 'm]
  end
|}]

let f (module A : Stack) = exclave_ A.alloc A.m ()

[%%expect{|
val f :
  (module Stack) @ [< global many read_write] ->
  string @ [> local aliased dynamic] = <fun>
|}]

(* [Stack] only promises a [local] *)
let f (module A : Stack) @ global = A.alloc A.m ()

[%%expect{|
Line 1, characters 36-50:
1 | let f (module A : Stack) @ global = A.alloc A.m ()
                                        ^^^^^^^^^^^^^^
Error: This value is "local" but is expected to be "global".
|}]
