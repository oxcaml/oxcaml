(* TEST
 flags = "-extension layouts_beta -extension mode";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

(* Record construction is a concrete value-shaped normal exit; it may join
   another value exit in a single direct-return layout through a local-region
   close. *)

external opaque_local : local_ 'a -> local_ 'a = "%opaque"

type boxed_record = { field : int }

type (_ : any) record_w =
  | Record_box : boxed_record record_w
  | Record_str : string record_w

let[@inline never] concrete_record : type (a : any). a record_w -> a =
  fun w ->
    let local_ x = ref 0 in
    let local_ _y = opaque_local x in
    match w with
    | Record_box -> { field = 0 }
    | Record_str -> "s"

type record_calls =
  { call_box : unit -> boxed_record;
    call_str : unit -> string
  }

let[@inline never] make_record_calls (f : ('a : any). 'a record_w -> 'a) =
  { call_box = (fun () -> f Record_box);
    call_str = (fun () -> f Record_str)
  }

let () =
  assert ((concrete_record Record_box).field = 0);
  assert (String.equal (concrete_record Record_str) "s");
  let calls = make_record_calls concrete_record in
  assert ((calls.call_box ()).field = 0);
  assert (String.equal (calls.call_str ()) "s");
  ()
