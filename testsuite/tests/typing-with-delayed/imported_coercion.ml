(* TEST
 readonly_files = "module_coercion.ml";
 modules = "module_coercion.ml";
 {
   bytecode;
 }{
   native;
 }
*)

module Type = Module_coercion.Use_type (Module_coercion.XT)
module Mixed = Module_coercion.Use_chained (Module_coercion.XC)
module Alias = Module_coercion.Use_alias (Module_coercion.Alias_value)
module Strengthened =
  Module_coercion.Use_present (Module_coercion.Strengthened_value)

let () =
  assert (Type.value = 42);
  assert (Mixed.answer = 42);
  assert (Alias.answer = 42);
  assert (Alias.tail = 17);
  assert (Strengthened.answer = 42);
  assert (Strengthened.tail = 17)
