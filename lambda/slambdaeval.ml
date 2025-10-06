module SL = Slambda

let do_eval ({ Lambda.code = SL.Quote lam } as p) = { p with code = lam }

let eval p = Profile.(record static_eval) do_eval p
