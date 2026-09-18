(* Exactly half the values are portable: two of four. A hoist must be strictly better than
   half, so suggestions stay per-item. *)
let counter = ref 0
let double x = x * 2
let triple x = x * 3
let bump x = x + !counter
let jump x = x - !counter
