(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

type r = { abc : int; mutable abd : string }
type u = #{ xyz : int; xyw : string }
type s = { c : u }

let i1 = (.ab)

let i2 : (r, _) idx_imm = (.ab)

let i3 = (.c.#xy)

let i4 : (r, string) idx_mut = (.ab)

let i5 : (r, int) idx_mut = (.abd)

let x : int = "string"
