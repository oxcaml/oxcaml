(* body_hash.ml, edited. *)

let[@inline never] apply f x = f x

let[@inline never] apply2 f x y = f x + y

let[@inline never] is_pos x = x > 0

let[@inline never] next x = x + 1

external blit : 'a array -> int -> 'a array -> int -> int -> unit = "%arrayblit"

let[@inline never] keep_constants x = if x > 20 then apply next x else 4

let[@inline never] keep_names x =
  let z = x * 2 in
  apply next z

let[@inline never] keep_attributes x = apply next x

let[@inline never] keep_closure_body n = apply (fun x -> (x * n) + 1) n

let[@inline never] keep_arguments x = apply next (x * 2)

let[@inline never] keep_and x = if x > 0 then apply is_pos x else false

let[@inline never] keep_raise x = if x > 0 then raise Not_found else x

let[@inline never] change_folding x = if apply is_pos x then 1 else 2

let[@inline never] change_not x = if apply is_pos x then apply next x else 0

let[@inline never] change_string_keys s =
  match s with "a" -> apply next 0 | "c" -> 1 | _ -> 2

let[@inline never] change_array_kind (a : string array) b = blit a 0 b 0 1

let[@inline never] change_arm x = if x > 0 then x else apply next x

let[@inline never] change_raise x = if x > 0 then x + 1 else x

let[@inline never] change_arity x = apply2 next x 1
