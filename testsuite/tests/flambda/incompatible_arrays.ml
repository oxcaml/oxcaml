(* TEST
 flambda2;
 native;
*)


type nonrec ('a : any mod separable) array = 'a array

type t1 = int64_u array
type t2 = float32_u array

external geti : int64_u array -> int -> int64_u = "%array_unsafe_get"
external getf : float32_u array -> int -> float32_u = "%array_unsafe_get"
external ignorei : int64_u -> unit = "%ignore"
external ignoref : float32_u -> unit = "%ignore"
external opaquei : int64_u -> int64_u = "%opaque"
external opaquef : float32_u -> float32_u = "%opaque"

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
