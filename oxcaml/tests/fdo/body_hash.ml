(* body_hash_edited.ml changes the [keep_*] functions in ways that must keep
   their body hashes, and the [change_*] functions in ways that must change
   them. *)

let[@inline never] apply f x = f x

let[@inline never] apply2 f x y = f x + y

let[@inline never] is_pos x = x > 0

let[@inline never] next x = x + 1

external blit : 'a array -> int -> 'a array -> int -> int -> unit = "%arrayblit"

(* Other constants *)
let[@inline never] keep_constants x = if x > 10 then apply next x else 3

(* Variable names *)
let[@inline never] keep_names x =
  let y = x * 2 in
  apply next y

(* Inlining attributes *)
let[@inline never] keep_attributes x = (apply [@inlined never]) next x

(* The body of a nested function *)
let[@inline never] keep_closure_body n = apply (fun x -> x + n) n

(* An argument without calls *)
let[@inline never] keep_arguments x = apply next x

(* [a && b] is translated like [if a then b else false] *)
let[@inline never] keep_and x = x > 0 && apply is_pos x

(* The exception raised *)
let[@inline never] keep_raise x = if x > 0 then raise Exit else x

(* An [if] with the arms [1] and [0] is folded into its condition *)
let[@inline never] change_folding x = if apply is_pos x then 1 else 0

(* [not] in a condition swaps the arms *)
let[@inline never] change_not x =
  if not (apply is_pos x) then apply next x else 0

(* The keys of a string switch determine its tests *)
let[@inline never] change_string_keys s =
  match s with "a" -> apply next 0 | "b" -> 1 | _ -> 2

(* The blit of an [int array] is expanded into loops *)
let[@inline never] change_array_kind (a : int array) b = blit a 0 b 0 1

(* A call moved to the other arm *)
let[@inline never] change_arm x = if x > 0 then apply next x else x

(* A raise, which changes how the counts relate *)
let[@inline never] change_raise x = if x > 0 then raise Exit else x
