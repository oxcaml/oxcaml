(* TEST
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

let x = (fun () -> (fun _ -> Lazy.force (lazy "")) ()) ()

let foo = lazy (fun x : int -> x)

let t = lazy (Lazy.force foo)
