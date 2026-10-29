(* TEST
   flambda;
   ocamlopt_flags = "-flambda2-expert-inline-effects-in-cmm";
   native;
*)

external signed_mod : nativeint -> nativeint -> nativeint
  = "%nativeint_mod"
external unsigned_mod : nativeint -> nativeint -> nativeint
  = "%nativeint_unsigned_mod"

let counter = ref 0

let[@inline never] f x =
  incr counter;
  x

let () =
  let result = signed_mod (f 42n) (1n) in
  Printf.printf "42 %% 1 = %nd; counter=%d\n" result !counter;
  let result = signed_mod (f 42n) Nativeint.min_int in
  Printf.printf "42 %% min_int = %nd; counter=%d\n" result !counter;
  let result = unsigned_mod (f 42n) (1n) in
  Printf.printf "42 %%u 1 = %nd; counter=%d\n" result !counter;
  let result = unsigned_mod (f 42n) (-1n) in
  Printf.printf "42 %%u max_int = %nd; counter=%d\n" result !counter;
