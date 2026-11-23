(* TEST
 readonly_files = "static_unit.ml";
 flags = "-extension layout_poly_alpha -nocwd -Ix .";
 setup-ocamlopt.byte-build-env;
 module = "static_unit.ml";
 ocamlopt.byte;
 module = "use_static_unit.ml";
 ocamlopt.byte;
 unset module;
 program = "${test_build_directory}/use_static_unit.exe";
 all_modules = "static_unit.cmx use_static_unit.cmx";
 ocamlopt.byte;
 run;
 check-program-output;
*)

external to_float : float# -> float = "%box_float"

(* [Static_unit.id] can be instantiated because [Static_unit] is static *)
let () =
  Printf.printf "id: %d %.1f\n" (Static_unit.id 1)
    (to_float (Static_unit.id #2.0))

(* Access dynamic items too *)
let () =
  incr Static_unit.counter;
  let cell = new Static_unit.cell in
  cell#set (!Static_unit.counter + 41);
  Printf.printf "counter: %d\ncell: %d\n" !Static_unit.counter cell#get
