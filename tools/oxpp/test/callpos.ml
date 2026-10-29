(* Implicit source positions become optional Lexing.position arguments. *)

let n ~(here : [%call_pos]) = here

let p () = [%src_pos]

type 'a t = here:[%call_pos] -> 'a
