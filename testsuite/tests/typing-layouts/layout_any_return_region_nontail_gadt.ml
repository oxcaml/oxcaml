(* TEST
 flags = "-extension layouts_beta -extension mode";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

(* GADT-refined concrete exits that share one sort (value) remain positive
   through the local-region close path. *)

external opaque_local : local_ 'a -> local_ 'a = "%opaque"

type ('a : any, 'b : any) eq = Refl : ('x : any). ('x, 'x) eq

type (_ : any) refine_w =
  | Refine_int : ('a, int) eq -> 'a refine_w
  | Refine_str : string refine_w

let[@inline never] concrete_gadt_refined : type (a : any). a refine_w -> a =
  fun w ->
    let local_ x = ref 0 in
    let local_ _y = opaque_local x in
    match w with
    | Refine_int Refl -> 0
    | Refine_str -> "s"

type refined_calls =
  { call_int : unit -> int;
    call_str : unit -> string
  }

let[@inline never] make_refined_calls
    (f : ('a : any). 'a refine_w -> 'a) =
  { call_int = (fun () -> f (Refine_int Refl));
    call_str = (fun () -> f Refine_str)
  }

let () =
  assert (concrete_gadt_refined (Refine_int Refl) = 0);
  assert (String.equal (concrete_gadt_refined Refine_str) "s");
  let calls = make_refined_calls concrete_gadt_refined in
  assert (calls.call_int () = 0);
  assert (String.equal (calls.call_str ()) "s");
  ()
