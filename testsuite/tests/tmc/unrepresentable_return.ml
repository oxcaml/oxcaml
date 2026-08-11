(* TEST
 flags = "-extension layouts_beta";
 expect;
*)

(* A [@tail_mod_cons] function whose return type has no representable layout
   is rejected at translation. *)
let[@tail_mod_cons] rec loop : type (a : any). unit -> a = fun () -> loop ()
[%%expect{|
Line 1, characters 24-76:
1 | let[@tail_mod_cons] rec loop : type (a : any). unit -> a = fun () -> loop ()
                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: [@tail_mod_cons]: Functions whose return type has no representable layout (it has layout any, or its result is only ever forwarded from other calls) cannot be [@tail_mod_cons]
|}]
