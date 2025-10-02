module SL = Slambda

let eval ({ Lambda.code = SL.Quote lam } as p) = { p with code = lam }
