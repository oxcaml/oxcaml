type program =
  { compilation_unit : Compilation_unit.t;
    main_module_block_format : Lambda.main_module_block_format;
    arg_block_idx : int option;
    code : Lambda.slambda
  }
