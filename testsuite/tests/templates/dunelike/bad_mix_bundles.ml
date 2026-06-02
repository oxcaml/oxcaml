module R1 = Bundle.Make(P_int)()
module R2 = Bundle.Make(P_int)()

(* R1.Basic.t and R2.Basic.t are fresh abstract types from two separate
   functor applications, so values cannot be mixed across them. *)
let _ : R1.Basic.t = R2.Basic.create 42
