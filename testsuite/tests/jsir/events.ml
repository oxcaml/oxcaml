(* TEST
 flags = "-g -djsir -nopervasives";
 jsir;
*)

(* Checks where the JSIR produced by Flambda 2 with [-g] has debug events,
   which js_of_ocaml turns into source locations. *)

external ( + ) : int -> int -> int = "%addint"
external ( +. ) : float -> float -> float = "%addfloat"
external float_of_int : int -> float = "%floatofint"
external ( = ) : int -> int -> bool = "%eq"
external raise : exn -> 'a = "%raise"
external opaque : 'a -> 'a = "%opaque"
external sys_getenv : string -> string = "caml_sys_getenv"

exception E of int

(* Function start; the runtime calls for [float_of_int] and [+.] get events;
   [+] does not. *)
let[@inline never] arith x = float_of_int (x + 1) +. 1.5

(* Non-tail call: events before and after. *)
let[@inline never] non_tail f x = f x + 1

(* Tail call: an event before, none after. *)
let[@inline never] tail f x = f x

(* C call (an [Apply] in Flambda): events before and after. *)
let[@inline never] c_call () = sys_getenv "HOME"

(* Raise: an event before. *)
let[@inline never] raise_e x = if x + 0 = 3 then raise (E x) else x

let () =
  let _ = opaque (arith 1) in
  let _ = opaque (non_tail (fun x -> x + 1) 2) in
  let _ = opaque (tail (fun x -> x + 2) 3) in
  let _ = opaque (c_call ()) in
  let _ = opaque (raise_e 4) in
  ()
