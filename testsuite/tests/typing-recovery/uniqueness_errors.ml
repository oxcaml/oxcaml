(* TEST
   flags = " -typing-recovery -extension-universe alpha";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let unique_use (_x @ local unique) = ()

(* Used uniquely twice. Unlike the errors below, this drops the binding, so we
   don't use [used_twice] below. *)
let used_twice (x @ unique) =
  unique_use x;
  unique_use x

(* [borrow_] out of a borrowing context *)
let borrow_out_of_context (y @ unique) =
  let _y0, _y1 = borrow_ y, borrow_ y in
  ()

(* Borrowed value used uniquely *)
let borrow_used_uniquely (y @ unique) = unique_use (borrow_ y)

(* Unique use during borrowing *)
let unique_use_during_borrow (y @ unique) =
  let _x = borrow_ y in
  unique_use y

(* Unique use of a value from outside a module *)
let boundary () =
  let x = "foo" in
  let module M = struct
    let y = (x : @ unique)
  end in
  ()

type t = A of string | B of string

(* Overwrite changing the tag *)
let swap = function
  | A s as v -> overwrite_ v with B s
  | B s as v -> overwrite_ v with A s

(* All of the above, except [used_twice], are still bound. *)
let _ : string @ unique -> unit = borrow_out_of_context
let _ : string @ unique -> unit = borrow_used_uniquely
let _ : string @ unique -> unit = unique_use_during_borrow
let _ : unit -> unit = boundary
let _ : t @ unique -> t = swap

(* This error should still be raised, despite the prior errors. *)
let t : string = 10
