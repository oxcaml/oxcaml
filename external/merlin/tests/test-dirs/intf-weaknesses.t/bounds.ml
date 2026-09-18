(* Every way a bound can involve a type other than the declaration's own parameters:
   hidden outright, exposed, exposed through an alias, mixed with a parameter, annotated
   on both sides of the veil, and reached through a functor application.
   [box]/[pair]/[masked]/[residue] are the bounds that do survive, on the parameters
   themselves. *)
module Hidden : sig
  type u
  type 'a tracked : immutable_data with 'a
end = struct
  type u = int
  type 'a tracked : immutable_data with 'a
end

module Exposed : sig
  type u
end = struct
  type u = int
end

(* The functor's result is sealed so the applied path stays abstract. *)
module Make_table (Key : sig
    type u
  end) : sig
  type 'a t : immutable_data with Key.u with 'a
end = struct
  type 'a t = { entries : (Key.u * 'a) list }
end

type 'a opaque_arg
type 'a tracked : immutable_data with 'a

(* CR-someday ggray: strengthen ml-side module ascriptions too; [Hidden]'s cap limits the
   suggestions for these declarations. *)
type public_u = Hidden.u
type from_hidden = { a : Hidden.u }
type from_exposed = { b : Exposed.u }
type from_alias = { c : public_u }

type partial =
  { hidden : Hidden.u
  ; pending : float opaque_arg
  }

type 'a both =
  { pub : 'a tracked
  ; priv : 'a Hidden.tracked
  }

type 'a from_functor = { table : 'a Make_table(Hidden).t }

module Int_u = struct
  type u = int
end

type 'a from_functor_immutable = { table : 'a Make_table(Int_u).t }
type 'a box = { value : 'a }

type ('a, 'b) pair =
  { left : 'a
  ; right : 'b
  }

type 'a masked = { value : 'a @@ portable }
type 'a residue = { payload : 'a @@ aliased global }
type 'a in_mutable = { mutable slot : 'a }
