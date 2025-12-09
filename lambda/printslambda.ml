let slambda = Printlambda.slambda

let program ppf { Slambda.code; _ } = slambda ppf code
