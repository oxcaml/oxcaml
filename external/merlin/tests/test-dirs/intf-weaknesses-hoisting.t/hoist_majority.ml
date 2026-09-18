(* Three of four values are provably portable; [bump] reads the toplevel ref through the
   portability lock. A portable majority should hoist the claim to a floating clause and
   exempt [bump] explicitly. *)
let counter = ref 0
let double x = x * 2
let triple x = x * 3
let quad x = x * 4
let bump x = x + !counter
