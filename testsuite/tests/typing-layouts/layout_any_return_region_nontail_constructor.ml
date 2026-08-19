(* TEST
 flags = "-extension layouts_beta -extension mode";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

(* A constructor-allocation result and another value result are both the same
   sort (value); the surrounding local region must close after preserving the
   single direct-return layout. *)

external opaque_local : local_ 'a -> local_ 'a = "%opaque"

type boxed_constructor = C of int

type (_ : any) constr_w =
  | Constr_box : boxed_constructor constr_w
  | Constr_str : string constr_w

let[@inline never] concrete_constructor : type (a : any). a constr_w -> a =
  fun w ->
    let local_ x = ref 0 in
    let local_ _y = opaque_local x in
    match w with
    | Constr_box -> C 0
    | Constr_str -> "s"

type constructor_calls =
  { call_box : unit -> boxed_constructor;
    call_str : unit -> string
  }

let[@inline never] make_constructor_calls
    (f : ('a : any). 'a constr_w -> 'a) =
  { call_box = (fun () -> f Constr_box);
    call_str = (fun () -> f Constr_str)
  }

let () =
  let (C direct) = concrete_constructor Constr_box in
  assert (direct = 0);
  assert (String.equal (concrete_constructor Constr_str) "s");
  let calls = make_constructor_calls concrete_constructor in
  let (C indirect) = calls.call_box () in
  assert (indirect = 0);
  assert (String.equal (calls.call_str ()) "s");
  ()
