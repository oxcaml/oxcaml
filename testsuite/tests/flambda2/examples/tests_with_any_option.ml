(* TEST
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-raw, dump-simplify;
 check-fexpr-dump;
*)

external ( + ) : int -> int -> int = "%addint"

external ( - ) : int -> int -> int = "%subint"

external ( > ) : 'a -> 'a -> bool = "%greaterthan"

external ( < ) : 'a -> 'a -> bool = "%lessthan"

external array_get : 'a array -> int -> 'a = "%array_safe_get"

external array_set : 'a array -> int -> 'a -> unit = "%array_safe_set"

let[@inline always] to_inline _x _y = 42

type ('a : any) my_option = Some of 'a | None

let f c m n x' y' =
  let x = if c < 0 then x' else x' + 1 in
  let y = if c < 0 then y' else y' + 1 in
  match m with
  | None -> 0
  | Some a -> ( match n with None -> 1 | Some b -> to_inline (x + y) (a + b))
