(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

(* Mode errors raised by [submode_err] outside of [typecore.ml]'s [submode]. *)

(* A structure item must be global. *)
let x = stack_ (1, 2)

(* A global closure can't capture a local value. *)
let f (x @ local) = ref (fun () -> x)

(* Same as the first one, but in a local module. *)
let g () =
  let module M = struct
    let y @ local = stack_ (ref 1)
  end in
  ()

(* [f] and [g] are still bound. *)
let _ : int @ local -> (unit -> int @ local) ref = f
let _ : unit -> unit = g

(* This error should still be raised, despite the prior errors. *)
let t : string = 10
