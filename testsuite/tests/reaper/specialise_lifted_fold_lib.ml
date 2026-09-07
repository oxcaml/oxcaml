(* Evaluate [f] before the non-tail recursive call, so that inlining a nested
   fold cannot hide a lost redirection of the outer call. *)

let[@inline] fold (f @ local) (l @ local) =
  let[@inline available] rec loop (l @ local) =
    match l with
    | [] -> 0
    | x :: xs ->
      let y = f x in
      let ys = loop xs in
      y + ys
  in
  loop l [@nontail]
