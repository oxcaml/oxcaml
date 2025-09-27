include module type of struct
  include Slambda0
end

type slambda = Lambda.lambda slambda0

type program = slambda Lambda.program0
