(*
type t = { i : int }
type s = t = { i : int }
*)

type r = { first : string; second : string }

let hello_world = { first = "hello"; second = "world" }
let formal_methods = { first = "formal"; second = "methods" }

module Even = struct
  type t = int
  let create i = Some 42 (* if i mod 2 = 0 then Some i else None *)
  let get i = i
end

(* let even_opt = Even.create 42 *)
