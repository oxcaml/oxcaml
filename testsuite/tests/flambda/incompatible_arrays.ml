(* TEST
 flambda2;
 native;
*)


type nonrec ('a : any) array = 'a array

type t1 = int64_u array
type t2 = float32# array

external geti : int64_u array -> int -> int64_u = "%array_unsafe_get"
external getf : float32# array -> int -> float32# = "%array_unsafe_get"
external ignorei : int64_u -> unit = "%ignore"
external ignoref : float32# -> unit = "%ignore"
external opaquei : int64_u -> int64_u = "%opaque"
external opaquef : float32# -> float32# = "%opaque"

type _ wit =
| A : t1 wit
| B : t2 wit

let[@inline] get : type a . a wit -> a -> int -> unit =
  fun wit x idx ->
  match wit with
  | A -> ignorei (opaquei (geti x idx))
  | B -> ignoref (opaquef (getf x idx))

let () =
  let a : t1 = [|#33L|] in
  get (Sys.opaque_identity A) a 0
