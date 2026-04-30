module R1 = Bundle.Func(P_int)()
module R2 = Bundle.Func(P_int)()

(* R1.Basic.t and R2.Basic.t are fresh abstract types from two separate
   functor applications, so values cannot be mixed across them. *)
let _ : R1.Basic.t = R2.Basic.create 42
