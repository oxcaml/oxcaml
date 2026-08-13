type t : B.k1

(* check that if a type expands to something in a missing cmi, it still gets
   kind any. *)
kind_ k_any = any
type s_any : k_any = B.t

(* check that if a type's kind expands to something in a missing cmi, it still
   gets kind any. *)
type s1 : any mod portable = t

(* But not a more specific kind. *)
type s2 : value mod portable = t
