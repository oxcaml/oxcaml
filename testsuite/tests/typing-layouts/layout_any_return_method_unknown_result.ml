(* TEST
 flags = "-extension layouts_beta";
 flambda2;
 setup-ocamlopt.byte-build-env;
 compiler_reference =
   "${test_source_directory}/layout_any_return_method_unknown_result.compilers.reference";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

class runner =
  object (self)
    method run : type (a : any). (unit -> a) -> unit -> a =
      fun f () -> f ()

    method run_self : type (a : any). (unit -> a) -> unit -> a =
      fun f () -> self#run f ()
  end

let[@inline never] forward_method
    : type (a : any). runner -> (unit -> a) -> unit -> a =
  fun o f () -> o#run f ()

let[@inline never] forward_self : type (a : any). (unit -> a) -> unit -> a =
  fun f () -> (new runner)#run_self f ()

let use_public () = forward_method (new runner) (fun () -> 42) ()

let use_self () = forward_self (fun () -> 42) ()
