(* TEST
 flags = "-extension layouts_beta";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

(* Partial applications of return-any functions in non-tail (region-closing)
   position: the compiler-generated partial-application wrapper forwards to
   the full function through its own return continuation, which must stay a
   tail call regardless of the region-close behaviour of the original
   partial-application site.  The optional-argument elimination eta-expansion
   generates exactly this shape. *)

let[@inline never] fwd_opt : ('a : any). ?tag:int -> (unit -> 'a) -> unit -> 'a
    =
 fun ?tag:_ f () -> f ()

let as_int : (unit -> int) -> unit -> int = fwd_opt

let as_int_nontail : (unit -> int) -> unit -> int =
 fun eta -> (fwd_opt ~tag:0 eta) [@nontail]

let[@inline never] fwd2 : type (a : any). int -> (unit -> a) -> a =
 fun _ f -> f ()

let partial_nontail () =
  let p = (fwd2 (Sys.opaque_identity 5)) [@nontail] in
  p (fun () -> 33)

let () =
  assert (as_int (fun () -> 42) () = 42);
  assert (as_int_nontail (fun () -> 27) () = 27);
  assert (partial_nontail () = 33)
