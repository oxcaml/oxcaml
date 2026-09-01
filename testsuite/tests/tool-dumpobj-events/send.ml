(* TEST
 flags = "-g";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 run;
 check-program-output;
*)

(* Like test.ml, but for method sends. *)

let o = object
  method m x = x + 1
  method uses (_ : (unit -> int) @ yielding) = 0
end

(* Non-tail unyielding send: expect "after/unyielding-call(2)" (the object
   counts as an argument). *)
let send_nontail () = o#m 41 + 1

(* Unyielding tail send: expect "pseudo/unyielding-call(2)" at APPTERM. *)
let send_tail () = o#m 42

(* A send passing a yielding argument may yield: expect a plain
   "after/ret(2)". *)
let send_yielding_arg (h : (unit -> int) @ yielding) = o#uses h + 1

(* Yielding tail send: no event at all. *)
let send_tail_yielding (h : (unit -> int) @ yielding) = o#uses h
