(* Counters of code that the compiler turns into something else (see dune).
   [choose] and [classify] are inlined at several call sites: with an unknown
   argument their switches stay; with a constant one only the arm that is taken
   remains. *)

let[@inline never] side x = x * 3

let[@inline always] choose b x = if b then side x else x + 1

let[@inline always] classify n x =
  match n with 0 -> x | 1 -> side x | 2 -> x * 7 | _ -> side (x + 1)

let[@inline never] kept b x = choose b x

let[@inline never] folded x = choose true x

let[@inline never] kept_match n x = classify n x

let[@inline never] folded_match x = classify 1 x

(* A self tail call that Flambda does not turn into a loop becomes a jump in the
   backend: its counter goes onto the branch edge leading to it. *)
let[@inline never] [@loop never] rec count_down n =
  if n <= 0 then n else count_down (n - 1)

(* Flambda turns a match with constant results into a table load: its counters
   go onto the edges before it, which are the function entry in [code], the
   [then] branch in [guarded], and the bounds check in [sum_codes]. *)
type t =
  | A
  | B
  | C
  | D

let[@inline never] code v = match v with A -> 1 | B -> 5 | C -> 2 | D -> 8

let[@inline never] guarded b v =
  if b then match v with A -> 1 | B -> 5 | C -> 2 | D -> 8 else 0

let[@inline never] sum_codes vs =
  let s = ref 0 in
  for i = 0 to Array.length vs - 1 do
    s := !s + match vs.(i) with A -> 1 | B -> 5 | C -> 2 | D -> 8
  done;
  !s

let () =
  let n = Sys.opaque_identity 2 in
  let v = Sys.opaque_identity C in
  Printf.printf "%d\n"
    (kept (n > 0) 1
    + folded 2 + kept_match n 3 + folded_match 4 + count_down n + code v
    + guarded (n > 0) v
    + sum_codes [| A; v; D |])
