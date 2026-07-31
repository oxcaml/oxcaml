(* TEST
 flags += "-extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

let use_portable (x @ portable) = x
[%%expect{|
val use_portable : 'a @ [< 'm & portable] -> 'a @ [> 'm] = <fun>
|}]

module M = struct
  let id x = x
end
[%%expect{|
module M : sig val id : 'a @ [< 'm] -> 'a @ [> 'm] end
|}]

let a (x @ portable) = use_portable    (M.id x)
let b (x @ nonportable) = use_portable (M.id x)
[%%expect{|
val a : 'a @ [< 'm & global portable] -> 'a @ [> 'm | dynamic] = <fun>
Line 2, characters 39-47:
2 | let b (x @ nonportable) = use_portable (M.id x)
                                           ^^^^^^^^
Error: This value is "nonportable" but is expected to be "portable".
|}]

module type S = sig
  type t

  val v : t
end
[%%expect{|
module type S = sig type t val v : t end
|}]

let make (type a) (x : a) : (module S with type t = a) =
  (module struct
    type t = a

    let v = x
  end)
[%%expect{|
val make :
  'a @ [< global many uncontended forkable unyielding read_write] ->
  (module S with type t = 'a) @ [> aliased nonportable stateful dynamic] =
  <fun>
|}]

let unpack_inferred_witness () =
  let e = make (fun (y : int ref) -> y) in
  let module M = (val e) in
  let l = local_ (ref 0) in
  let _ = M.v l in
  ()
[%%expect{|
val unpack_inferred_witness : unit @ 'n -> unit @ 'm = <fun>
|}]

let unpack_annotated_witness () =
  let e : (module S with type t = int ref -> int ref) =
    make (fun y -> y)
  in
  let module M = (val e) in
  let l = local_ (ref 0) in
  let _ = M.v l in
  ()
[%%expect{|
Line 7, characters 14-15:
7 |   let _ = M.v l in
                  ^
Error: This value is "local" but is expected to be "global".
|}]
