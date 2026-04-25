module R1 = Bundle_share.Func(P_int)()
module R2 = Bundle_share.Func(P_int)()

(* R1.Basic_share.t and R2.Basic_share.t are fresh abstract types,
   so values from different applications cannot be mixed. *)
let _ : R1.Basic_share.t = R2.Basic_share.create ()
