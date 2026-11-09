(* [id] comes from [C], opened with [-open-cmi] or [-open-cmi-x]. Instantiating
   it requires [C] to be static, which requires a guaranteed cmx. *)
let _ = id 42
