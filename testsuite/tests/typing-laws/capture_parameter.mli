(* The law of [P] refers to the unit [Captured]; the parameter [Captured]
   of [F] must not capture it in the generated choice module type. *)
module type P = sig law? l : Captured.p end
module F (Captured : sig end) (X : sig include P end) : sig
  law? l : true
end
