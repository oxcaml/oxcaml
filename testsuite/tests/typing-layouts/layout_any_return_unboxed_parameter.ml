(* TEST
 flambda2;
 flags = "-extension layouts_beta -no-flambda2-reaper";
 {
   ocamlopt_flags = "-Oclassic";
   compiler_directory_suffix = ".Oclassic";
   native;
 }{
   ocamlopt_flags = "-O3";
   compiler_directory_suffix = ".O3";
   native;
 }{
   native;
 }
*)

external box_float : float# -> float = "%box_float"
external raise_any : ('result : any). exn -> 'result = "%raise"

let[@inline never] forward : type (result : any).
    float -> (unit -> result) -> result =
  fun (argument [@unboxable]) callback ->
    ignore (Sys.opaque_identity argument);
    callback ()

let[@inline never] never_returns : type (result : any). float -> result =
  fun (argument [@unboxable]) ->
    ignore (Sys.opaque_identity argument);
    raise_any Exit

(* Unboxing a local parameter requires a region wrapper, which cannot clean up
   after a call with an unknown result layout. *)
let[@inline never] forward_local : type (result : any).
    float @ local -> (unit -> result) -> result =
  fun (argument [@unboxable]) callback ->
    let _ = Sys.opaque_identity argument in
    callback ()

let[@inline never] never_returns_local : type (result : any).
    float @ local -> result =
  fun (argument [@unboxable]) ->
    let _ = Sys.opaque_identity argument in
    raise_any Exit

let[@unboxable] increment (argument [@unboxable]) = argument +. 1.0

let[@inline never] forward_int (argument [@unboxable]) callback : int =
  ignore (Sys.opaque_identity (argument : float));
  callback ()

let () =
  assert (Int.equal 17 (forward 1.5 (fun () -> 17)));
  assert (Float.equal 2.5 (box_float (forward 1.5 (fun () -> #2.5))));
  let #(integer, floating) = forward 1.5 (fun () -> #(17, #2.5)) in
  assert (Int.equal 17 integer);
  assert (Float.equal 2.5 (box_float floating));
  assert (Int.equal 29 ((Sys.opaque_identity forward) 1.5 (fun () -> 29)));
  assert (Float.equal 3.5
            (box_float ((Sys.opaque_identity forward) 1.5 (fun () -> #3.5))));
  (match (never_returns 1.5 : int) with
  | exception Exit -> ()
  | _ -> assert false);
  (match ((Sys.opaque_identity never_returns) 1.5 : float#) with
  | exception Exit -> ()
  | _ -> assert false);
  assert (Float.equal 2.5 (increment 1.5));
  assert (Float.equal 3.5 ((Sys.opaque_identity increment) 2.5));
  assert (Int.equal 17 (forward_int 1.5 (fun () -> 17)));
  assert (Int.equal 29 ((Sys.opaque_identity forward_int) 1.5 (fun () -> 29)));
  assert (Int.equal 17 (forward_local 1.5 (fun () -> 17)));
  assert (Float.equal 2.5 (box_float (forward_local 1.5 (fun () -> #2.5))));
  let #(integer, floating) = forward_local 1.5 (fun () -> #(17, #2.5)) in
  assert (Int.equal 17 integer);
  assert (Float.equal 2.5 (box_float floating));
  assert (Int.equal 29
            ((Sys.opaque_identity forward_local) 1.5 (fun () -> 29)));
  assert (Float.equal 3.5
            (box_float
               ((Sys.opaque_identity forward_local) 1.5 (fun () -> #3.5))));
  let #(integer, floating) =
    (Sys.opaque_identity forward_local) 1.5 (fun () -> #(29, #3.5))
  in
  assert (Int.equal 29 integer);
  assert (Float.equal 3.5 (box_float floating));
  (match (never_returns_local 1.5 : int) with
  | exception Exit -> ()
  | _ -> assert false);
  (match ((Sys.opaque_identity never_returns_local) 1.5 : float#) with
  | exception Exit -> ()
  | _ -> assert false)
