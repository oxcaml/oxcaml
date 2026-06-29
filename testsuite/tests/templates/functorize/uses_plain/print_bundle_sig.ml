(* Print the expanded type of the bundle when applied with [P_int].  Used
   to verify (via [ocamlc -i]) that [Plain] and [Basic[P:P_int]] don't end
   up interned in the bundle's signature — they stay as global paths. *)

module type T = module type of struct
  include Bundle_uses_plain.Make (P_int) ()
end
