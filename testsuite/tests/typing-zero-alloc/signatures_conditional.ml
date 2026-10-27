(* TEST
   expect.opt;
*)

let[@zero_alloc conditional(arity 2, n/a, n/a)] apply2 f x y = f x y
[%%expect{|
val apply2 : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c [@@zero_alloc conditional]
|}]

module type Apply2 = sig
  val call : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
  [@@zero_alloc conditional]
end
[%%expect{|
module type Apply2 = sig
  val call : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
  [@@zero_alloc conditional]
end
|}]

module Apply2 : Apply2 = struct
  let call f x y = f x y
end
[%%expect{|
module Apply2 : Apply2
|}]

module UnderApply2 : Apply2 = struct
  let call f x y = let[@inline never] g = f x in g y
end
[%%expect{|
(* CR wsturgeon for wsturgeon: this needs to fail (under-application) *)
|}]

module type ApplyPartial = sig
  val apply_partial : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
  [@@zero_alloc conditional(arity 1, n/a, n/a)]
end
[%%expect{|
module type ApplyPartial = sig
  val apply_partial : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
  [@@zero_alloc conditional(arity 1, n/a, n/a)]
end
|}]

module OverApplyPartial : ApplyPartial = struct
  let apply_partial f x y = f x y
end
[%%expect{|
(* CR wsturgeon for wsturgeon: this needs to fail (over-application) *)
|}]

module ApplyPartial : ApplyPartial = struct
  let apply_partial f x = f x
end
[%%expect{|
module ApplyPartial : ApplyPartial
|}]

(* CR-someday wsturgeon: lowered argument differences, e.g. unboxed tuples? *)
