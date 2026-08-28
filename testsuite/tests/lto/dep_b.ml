let[@inline never] add_via_a x y = Dep_a.read (Dep_a.make x) + y

(* Applying the closure inlines [add]'s body here (this needs
   -flambda2-result-types-all-functions so that the closure's code id is known
   at the apply site), so this unit projects a value slot of a closure whose
   layout is decided when dep_a is rebuilt. *)
let[@inline never] apply_adder x y = Dep_a.make_adder x y
