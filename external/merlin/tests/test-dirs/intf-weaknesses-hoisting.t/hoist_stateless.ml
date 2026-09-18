(* Identity-shaped functions close over nothing and earn [stateless], which delivers
   [portable] to clients by implication, so the hoisted clause is [@@ stateless] alone.
   Exemptions must mind implications too: written [stateful] implies [nonportable], fine
   for [bump] (a ref-reader, nonportable anyway) but a silent weakening for [plus1]
   (portable; it closes over the stdlib [+], stateful only until the stdlib is annotated),
   whose exemption must therefore re-claim [portable]. *)
let counter = ref 0
let id x = x
let same x = x
let pick b x y = if b then x else y
let plus1 x = x + 1
let bump x = x + !counter
