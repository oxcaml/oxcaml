let x = B.x + 1

module A_alias = A

(* Typing [x] loads [A.cmi] through [-H liba].  The alias must still report
   warning 49 because direct references to [A] are not allowed. *)
