external select : bool -> int -> int -> int = "" "caml_csel_value"
  [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]

let[@local never] [@inline never] csel cond ifso ifnot = select cond ifso ifnot

let[@local never] [@inline never] min x y = select (x < y) x y

let () =
  Format.printf "csel false 1 2 = %d\n" (csel false 1 2);
  Format.printf "csel true 3 4 = %d\n" (csel true 3 4);
  Format.printf "min 2 3 = %d\n" (min 2 3);
  Format.printf "min 3 2 = %d\n" (min 3 2);
  Format.printf "min 1 1 = %d\n" (min 1 1)
