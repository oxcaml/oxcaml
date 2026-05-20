(* TEST
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

let do_stuff env = assert false

let stuff env =
  (* The call to [do_stuff] used to prevent data_flow from
     seeing that [r] is unused *)
  let r = ref None in
  for i = 0 to 3 do
    try
      (Sys.opaque_identity do_stuff) env;
      r := Sys.opaque_identity (Some 12)
    with _ ->
      r := Sys.opaque_identity None;
      ()
  done
