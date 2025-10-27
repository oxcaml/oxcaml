module Config = Config
module Parse_info = Parse_info
module StringSet = Stdlib.StringSet
module Targetint = Targetint
module Unit_info = Unit_info

module Jsir = struct
  include Code
  include Jsir
end

module Js_backend = struct
  type program =
    { program : Jsir.program;
      imported_compilation_units : Compilation_unit.Set.t
    }
end
