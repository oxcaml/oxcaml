(* The counters after Simplify of a continuation specialized for each of its
   uses: [get]'s match is a join point, specialized for the [Some] and [None]
   arms, which then fold it. The counters of each specialized copy go onto the
   edge of its use, even when that use follows a call rather than being the arm
   of a switch. *)

let[@inline never] side x = x * 3

let[@inline] get = function None -> 0 | Some x -> x + 1

let[@inline never] specialized c y =
  let o =
    if c
    then
      let z = side y in
      Some z
    else None
  in
  get o
