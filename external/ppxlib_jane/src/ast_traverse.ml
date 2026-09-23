class iter =
  object
    inherit Ppxlib_traverse_builtins.iter
    inherit Shim.Ast_traverse.iter
  end

class map =
  object
    inherit Ppxlib_traverse_builtins.map
    inherit Shim.Ast_traverse.map
  end

class ['acc] fold =
  object
    inherit ['acc] Ppxlib_traverse_builtins.fold
    inherit ['acc] Shim.Ast_traverse.fold
  end

class ['acc] fold_map =
  object
    inherit ['acc] Ppxlib_traverse_builtins.fold_map
    inherit ['acc] Shim.Ast_traverse.fold_map
  end

class ['ctx] map_with_context =
  object
    inherit ['ctx] Ppxlib_traverse_builtins.map_with_context
    inherit ['ctx] Shim.Ast_traverse.map_with_context
  end

class virtual ['res] lift =
  object
    inherit ['res] Ppxlib_traverse_builtins.lift
    inherit ['res] Shim.Ast_traverse.lift
  end

class virtual ['ctx, 'res] lift_map_with_context =
  object
    inherit ['ctx, 'res] Ppxlib_traverse_builtins.lift_map_with_context
    inherit ['ctx, 'res] Shim.Ast_traverse.lift_map_with_context
  end
