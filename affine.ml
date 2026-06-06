type ('a : value non_float) t =
  { mutable foo : 'a; mutable bar : 'a }

external get : ('a : value_or_null) ('b : value non_float).
  'a -> ('a, 'b) idx_mut -> 'b = "%get_idx"

(*
let safe_get1 is_bar t =
  let idx = (Core.Bool.select[@kind bits64]) is_bar (.bar) (.foo) in
  get t idx
*)

let safe_get2 is_bar t =
  let idx = if is_bar then (.bar) else (.foo) in
  get t idx

let fast_get is_bar (t : 'a t) =
  let t : 'a array = Obj.magic t in
  let i : int = Obj.magic is_bar in
  Array.unsafe_get t i
