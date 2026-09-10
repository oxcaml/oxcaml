(* TEST
 flags = "-extension layouts_beta -extension mode";
 flambda2;
 setup-ocamlopt.byte-build-env;
 compiler_reference =
   "${test_source_directory}/layout_any_return_try_region_unknown_result.compilers.reference";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

type (_ : any) witness =
  | Int : int witness
  | String : string witness

let[@inline never] poly : type (a : any). a witness -> unit -> a =
  fun w () ->
    match w with
    | Int -> 42
    | String -> "x"

let[@inline never] forward_try_region : type (a : any). (unit -> a) -> a =
  fun f ->
    try
      let local_ _x = ref 0 in
      f ()
    with _ -> f ()

let use_int () = forward_try_region (poly Int)
