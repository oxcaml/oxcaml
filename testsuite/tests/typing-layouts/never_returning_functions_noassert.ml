(* TEST
 flags += "-noassert";
 expect;
*)

(* [assert false] raises even with [-noassert], so it still never returns. *)

let literal () = assert false
let parenthesised () = assert (false)
let constrained () = assert (false : bool)
let computed () = assert (not true)
[%%expect{|
val literal : ('a : any). unit -> 'a = <fun>
val parenthesised : ('a : any). unit -> 'a = <fun>
val constrained : ('a : any). unit -> 'a = <fun>
val computed : unit -> unit = <fun>
|}]

let outcome =
  match (literal () : float#) with
  | _ -> "returned"
  | exception Assert_failure _ -> "raised"
[%%expect{|
val outcome : string = "raised"
|}]
