(* TEST
 expect;
*)

(* regression test: see PR#7298 *)
module M : sig
  val f : ('a : value mod portable). 'a -> 'a @ portable
end = struct
  let f x = (x : _ @ nonportable)
end
[%%expect{|
module M : sig val f : ('a : value mod portable). 'a -> 'a @ portable end
|}]
