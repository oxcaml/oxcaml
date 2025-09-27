let eval ({ Lambda.code = Slambda.SLquote lam } as p) = { p with code = lam }
