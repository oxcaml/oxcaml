let slambda = Printslambda0.slambda0 Printlambda.lambda

let program ppf { Slambda.code; _ } = slambda ppf code
