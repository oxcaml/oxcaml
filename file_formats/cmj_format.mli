type compilation_unit_descr =
  { cu_name: Compilation_unit.t;
    cu_pos: int;
    cu_codesize: int;
    cu_imports: Import_info.t array }

type library =
  { lib_units: compilation_unit_descr list }