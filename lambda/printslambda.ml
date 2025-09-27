let slambda = Printslambda0.slambda0 Printlambda.lambda

let program ppf { Lambda.code; _ } = slambda ppf code
