(* Paired with link_units.ml: a unit whose symbols another unit imports. *)

let () = print_endline "init a"

let counter = ref 0

let[@inline never] incr_counter () =
  incr counter;
  !counter

let[@inline] add x y = x + y

exception Error of string

let raise_error s = raise (Error s)

type t = { name : string; value : int }

let constant = { name = "constant"; value = 42 }

(* A symbol defined in a branch: the other branch raises, so the definition
   still dominates the end of the unit. *)
let branchy =
  if Sys.opaque_identity true then { name = "branchy"; value = 1 }
  else failwith "unreachable"

(* Recursive closures sharing a set of closures. *)
let rec even n = if n = 0 then true else odd (n - 1)

and odd n = if n = 0 then false else even (n - 1)

(* A cyclic value. *)
let rec cycle = 1 :: 2 :: cycle

module Inner = struct
  let f x = x * 2
end
