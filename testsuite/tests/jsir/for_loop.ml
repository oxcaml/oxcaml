(* TEST
 flags = "-djsir -nopervasives";
 jsir;
*)

(* Checks that the counters of "for" loops are plain integers in the JSIR
   produced by Flambda 2. Native code keeps them as naked int64s to get an
   extra bit of range (see [Lambda_to_lambda_transforms.rec_catch_for_for_loop]),
   but JSIR represents naked int64s as heap-allocated objects whose
   arithmetic goes through the runtime ([caml_int64_add], ...), which makes
   such loops very slow. Instead the counter is compared to the bound before
   being incremented, so neither loop below mentions an int64. *)

external ( + ) : int -> int -> int = "%addint"
external ( - ) : int -> int -> int = "%subint"
type 'a ref = { mutable contents : 'a }
external ref : 'a -> 'a ref = "%makemutable"
external ( ! ) : 'a ref -> 'a = "%field0"
external ( := ) : 'a ref -> 'a -> unit = "%setfield0"

let[@inline never] sum_up bound =
  let s = ref 0 in
  for i = 0 to bound do
    s := !s + i
  done;
  !s

let[@inline never] sum_down bound =
  let s = ref 0 in
  for i = bound downto 0 do
    s := !s - i
  done;
  !s
