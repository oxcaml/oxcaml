(* Escaping closures cannot adopt the lifted calling convention. Exercise
   both the escaped indirect call and the direct call at the creation site. *)
let[@inline] single f n =
  let[@inline never] rec loop n =
    if n <= 0 then f n else f n + loop (n - 1)
  in
  Sys.opaque_identity loop, loop n

(* Only [helper] is returned, but it shares a set of closures with [loop]. *)
let[@inline] mixed f g n =
  let[@inline never] rec loop n =
    if n <= 0 then f n else f n + helper (n - 1)
  and[@inline never] helper n =
    if n <= 0 then g n else g n + loop (n - 1)
  in
  Sys.opaque_identity helper, loop n
