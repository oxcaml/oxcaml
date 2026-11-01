(* The flags every module of the standard library is compiled with. Shared by
   the compiler's own build (gen.ml) and the build of the standard library for
   ocamlopt -target js_of_ocaml (external/ast-dependent-libs/stdlib-jsir). *)
let flags =
  [ "-nopervasives"; "-directory"; "stdlib"; "-strict-sequence"; "-g";
    "-absname"; "-extension"; "runtime_metaprogramming"; "-nostdlib";
    "-safe-string"; "-strict-formats"; "-no-alias-deps";
    "-w"; "+a-4-9-40-41-42-44-45-48-66-67-70"; "-w"; "-221"; "-principal" ]
