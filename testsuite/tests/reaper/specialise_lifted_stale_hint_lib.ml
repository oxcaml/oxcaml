(* [escape] prevents unboxing [r] until the caller removes the else branch.
   [Sys.opaque_identity] prevents the reaper from unboxing [captured1] in the
   control, where it is not stored in [r]. *)

let sink : (int * int) ref ref = ref (ref (0, 0))
let[@inline never] escape r = sink := r

let outer_hint x b =
  let captured1 = (x, 1) in
  let captured2 = (x, 2) in
  let r = ref captured1 in
  let[@inline never] rec g1 y = fst (Sys.opaque_identity captured1) + g2 y
  and[@inline never] g2 y = fst captured2 + y in
  if b then begin
    r := captured2;
    g2 3
  end else begin
    escape r;
    g1 4
  end

(* This wrapper looks small after lifting, but copying its helper is not
   cheap when [f] remains unknown. *)
let large f n =
  let[@inline never] rec large_loop n =
    let a = f n in
    let b = f a in
    let c = f b in
    let d = f c in
    let e = f d in
    let g = f e in
    let h = f g in
    let i = f h in
    let j = f i in
    let k = f j in
    let l = f k in
    let m = f l in
    let o = f m in
    let p = f o in
    let q = f p in
    let r = f q in
    if n <= 0 then r else r + large_loop (n - 1)
  in
  large_loop n

let outer_control x b =
  let captured1 = (x, 1) in
  let captured2 = (x, 2) in
  let r = ref captured2 in
  let[@inline never] rec g1 y = fst (Sys.opaque_identity captured1) + g2 y
  and[@inline never] g2 y = fst captured2 + y in
  if b then begin
    r := captured2;
    g2 3
  end else begin
    escape r;
    g1 4
  end
