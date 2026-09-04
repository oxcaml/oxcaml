(* TEST
 compile_only = "true";
 ocamlopt_flags = "-O3";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
*)

exception E

let[@zero_alloc assume strict] div_exact a b = a / b

let[@zero_alloc assume strict] nth (a : int array) i = a.(i)

let[@zero_alloc] raise_e () = raise E

let[@zero_alloc assume] reraise f x = try f x with e -> raise e

let catch g x = try g x with _ -> 0

let notrace_path f x = try f x with Not_found -> raise_notrace E
