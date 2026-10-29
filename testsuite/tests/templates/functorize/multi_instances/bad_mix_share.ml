module R1 = Bundle_opaque.Make(P_int)()
module R2 = Bundle_opaque.Make(P_int)()

(* R1.Basic_opaque.t and R2.Basic_opaque.t are fresh abstract types,
   so values from different applications cannot be mixed. *)
let _ : R1.Basic_opaque.t = R2.Basic_opaque.create ()
