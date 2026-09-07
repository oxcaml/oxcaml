[@@@ocaml.flambda_o3]

(* Model of [Base.List]'s stack map: an inline wrapper around a
   non-tail-recursive loop, which the reaper lifts. Callers must still be able
   to specialise the loop on the callback. *)

let[@inline] map_stack (f @ local) (l @ local) = exclave_
  let[@inline available] rec loop (l @ local) =
    match l with
    | [] -> []
    | x :: xs ->
      exclave_
      let y = f x in
      let ys = loop xs in
      y :: ys
  in
  loop l
