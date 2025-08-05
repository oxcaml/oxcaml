(* TEST
 include stdlib_upstream_compatible;
 reference = "${test_source_directory}/basics.reference";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

module My_module = struct
  let foo = "a"
  let bar = #5L
  let baz = "y"
  let qux = 10
end

let _ = print_int (Stdlib_upstream_compatible.Int64_u.to_int My_module.bar)
let _ = print_endline ""
let _ = print_int My_module.qux
let _ = print_endline ""
