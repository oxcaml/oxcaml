(* TEST
 flags = "-extension layouts_beta";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

(* Never-returning builtin primitives may be declared with a result type of
   layout [any].  Applying one in return position of a return-any function
   classifies the function as never-returning (Bottom result arity). *)

external my_raise : ('a : any). exn -> 'a = "%raise"
external my_reraise : ('a : any). exn -> 'a = "%reraise"
external my_raise_notrace : ('a : any). exn -> 'a = "%raise_notrace"
external my_raise_with_backtrace : ('a : any).
  exn -> Printexc.raw_backtrace -> 'a = "%raise_with_backtrace"

exception E

let[@inline never] never_returns : type (a : any). unit -> a =
  fun () -> my_raise E

let[@inline never] never_returns_notrace : type (a : any). unit -> a =
  fun () -> my_raise_notrace E

let[@inline never] never_returns_bt : type (a : any). unit -> a =
  fun () -> my_raise_with_backtrace E (Printexc.get_raw_backtrace ())

(* First-class use eta-expands to a never-returning wrapper. *)
let first_class : type (a : any). exn -> a = my_reraise

(* Alias-bound uses are not recognized as never-returning; they compile
   through the tail-forwarder (Unknown result) path instead. *)
let alias_forwarder : type (a : any). unit -> a =
  fun () -> let r = my_reraise in r E

(* Over-application: the raise's result instantiates to a function type. *)
let[@inline never] over_applied () : int =
  (my_raise E 42) [@warning "-ignored-extra-argument"]

let raises_e (f : unit -> 'b) =
  match f () with
  | exception E -> ()
  | _ -> assert false

let raises_e_float64 (f : unit -> float#) =
  match f () with
  | exception E -> ()
  | _ -> assert false

let () =
  (* value instantiations *)
  raises_e (fun () -> never_returns ());
  raises_e (fun () -> never_returns_notrace ());
  raises_e (fun () -> never_returns_bt ());
  raises_e (fun () -> first_class E);
  raises_e (fun () -> alias_forwarder ());
  raises_e (fun () -> over_applied ());
  (* non-value instantiations *)
  raises_e_float64 (fun () -> never_returns ());
  raises_e_float64 (fun () -> never_returns_notrace ());
  raises_e_float64 (fun () -> never_returns_bt ());
  raises_e_float64 (fun () -> first_class E);
  raises_e_float64 (fun () -> alias_forwarder ());
  (* concrete instantiation inside an ordinary function *)
  assert ((try if Sys.opaque_identity true then my_raise E else 1
           with E -> 2) = 2);
  ()
