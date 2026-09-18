(* TEST
 ocamlc_byte_exit_status = "0";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

[@@@implicit_kind: ('a : bits64)]

module M = struct
  [@@@implicit_kind: ('b : bits32)]

  let value = 0
end

let[@implicit_kind: ('c : immediate)] f x = x

class type s = object
  [@@@implicit_kind: ('a : immediate)]
  method f : unit -> 'a
end
