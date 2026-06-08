(* Two real modules of the same name still clash, even alongside [-alias Foo
   Bar]: letting hidden aliases coexist must not weaken uniqueness of visible
   components. *)
module M = struct end
module M = struct end
