(* TEST
   ocamlrunparam += ",Xmain_stack_size=786432,Xfiber_stack_size=262144";
   no-stack-checks;
   flags = "-runtime-variant=d";
   native;
*)

(* With guard-page stacks, the main stack takes its size from Xmain_stack_size,
   which may not be a power-of-two multiple of the fiber stack size. This used
   to trip an assertion in the debug runtime. *)
