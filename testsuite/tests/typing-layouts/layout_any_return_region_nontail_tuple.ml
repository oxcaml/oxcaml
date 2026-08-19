(* TEST
 flags = "-extension layouts_beta -extension mode";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

(* Tuple construction is a concrete value-shaped normal exit; it may join
   another value exit in a single direct-return layout through a local-region
   close. *)

external opaque_local : local_ 'a -> local_ 'a = "%opaque"

type (_ : any) tuple_w =
  | Tuple_box : (int * string) tuple_w
  | Tuple_str : string tuple_w

let[@inline never] concrete_tuple : type (a : any). a tuple_w -> a =
  fun w ->
    let local_ x = ref 0 in
    let local_ _y = opaque_local x in
    match w with
    | Tuple_box -> 0, "x"
    | Tuple_str -> "s"

type tuple_calls =
  { call_box : unit -> int * string;
    call_str : unit -> string
  }

let[@inline never] make_tuple_calls (f : ('a : any). 'a tuple_w -> 'a) =
  { call_box = (fun () -> f Tuple_box);
    call_str = (fun () -> f Tuple_str)
  }

let () =
  assert (concrete_tuple Tuple_box = (0, "x"));
  assert (String.equal (concrete_tuple Tuple_str) "s");
  let calls = make_tuple_calls concrete_tuple in
  assert (calls.call_box () = (0, "x"));
  assert (String.equal (calls.call_str ()) "s");
  ()
