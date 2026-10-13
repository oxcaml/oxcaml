(* TEST
   readonly_files = "conditional_lib.mli conditional_lib.ml";
   setup-ocamlopt.opt-build-env;
   flags = "-g -opaque";
   module = "conditional_lib.mli";
   ocamlopt.opt;
   module = "conditional_lib.ml";
   ocamlopt.opt;
   flags = "-g -I ocamlopt.opt ocamlopt.opt/conditional_lib.cmx";
   expect.opt;
*)

let[@zero_alloc] opaque_nonallocating_callback x =
  Conditional_lib.apply Conditional_lib.identity x
[%%expect{|
CR wsturgeon: this has to pass
|}]

let[@zero_alloc] opaque_two_argument_callback x y =
  Conditional_lib.apply2 Conditional_lib.add_ints x y
[%%expect{|
CR wsturgeon: this has to pass
|}]

let[@zero_alloc] opaque_allocating_callback x =
  Conditional_lib.apply Conditional_lib.allocate_ref x
[%%expect{|
CR wsturgeon: this has to fail
|}]

let[@zero_alloc] opaque_allocating_two_argument_callback x y =
  Conditional_lib.apply2 Conditional_lib.allocate_pair x y
[%%expect{|
CR wsturgeon: this has to fail
|}]

let[@zero_alloc] opaque_allocating_callback_argument x =
  Conditional_lib.apply Conditional_lib.identity (ref x)
[%%expect{|
CR wsturgeon: this has to fail
|}]
