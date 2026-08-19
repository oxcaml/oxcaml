(* TEST
 flags = "-extension layouts_beta -extension mode";
 flambda2;
 setup-ocamlopt.byte-build-env;
 compiler_reference =
   "${test_source_directory}/layout_any_return_nested_any_match_nontail.compilers.reference";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

type (_ : any) direct_w =
  | Direct_int : int direct_w
  | Direct_unknown : ('a : any). (local_ int ref -> 'a) -> 'a direct_w

type (_ : any) nested_w =
  | Nested : ('a : any). 'a direct_w -> 'a nested_w

external opaque_local : local_ 'a -> local_ 'a = "%opaque"

let[@inline never] nested_non_tail_unknown
    : type (a : any). a nested_w -> a =
  fun w ->
    match w with
    | Nested inner ->
      match inner with
      | Direct_int -> 42
      | Direct_unknown f ->
        let local_ x = ref 0 in
        let local_ x = opaque_local x in
        f x [@nontail]
