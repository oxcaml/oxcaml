(* TEST
 flags = "-extension layouts_beta -extension mode";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

(* Literal leaves that share one sort (value) form a single direct-return
   layout.  The local allocation keeps the region-close path covered while the
   leaves remain ordinary concrete results. *)

external opaque_local : local_ 'a -> local_ 'a = "%opaque"

type (_ : any) lit_w =
  | Lit_int : int lit_w
  | Lit_str : string lit_w

let[@inline never] concrete_literal : type (a : any). a lit_w -> a =
  fun w ->
    let local_ x = ref 0 in
    let local_ _y = opaque_local x in
    match w with
    | Lit_int -> 0
    | Lit_str -> "s"

type literal_calls =
  { call_int : unit -> int;
    call_str : unit -> string
  }

let[@inline never] make_literal_calls (f : ('a : any). 'a lit_w -> 'a) =
  { call_int = (fun () -> f Lit_int);
    call_str = (fun () -> f Lit_str)
  }

let () =
  assert (concrete_literal Lit_int = 0);
  assert (String.equal (concrete_literal Lit_str) "s");
  let calls = make_literal_calls concrete_literal in
  assert (calls.call_int () = 0);
  assert (String.equal (calls.call_str ()) "s");
  ()
