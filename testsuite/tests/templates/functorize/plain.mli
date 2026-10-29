(* A non-parameterised module.  Used to test that the functorizer stops
   chasing alias chains at non-parameterised compunits and keeps the
   reference as a global rather than interning the module into the bundle. *)
type t = string
val greeting : t
