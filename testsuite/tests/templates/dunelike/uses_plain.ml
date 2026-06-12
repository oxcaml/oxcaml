(* Parameters: P *)

(* Parameterised module that references a non-parameterised compunit
   ([Plain]) in its *signature*: the [greet] return type is [Plain.t].
   The functorizer should NOT include [Plain] in the bundle — path
   compression stops as soon as it sees a non-parameterised root, and
   leaves the [Plain.t] reference in the bundle's signature as a global. *)
let greet (_p : P.t) : Plain.t = Plain.greeting

(* Also test a direct module alias to a non-parameterised module *)
module Plain_alias = Plain
