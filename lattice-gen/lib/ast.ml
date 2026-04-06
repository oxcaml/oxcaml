type direction =
  | Lt
  | Gt

type chain =
  | Singleton of string Location.located
  | Chain of
      { direction : direction;
        names : string Location.located list;
        loc : Location.t
      }

type lattice_expr =
  { lattice : string Location.located;
    opposite : bool
  }

type field =
  { name : string Location.located;
    ty : lattice_expr
  }

type primitive_mapping =
  { source : string Location.located;
    target : string Location.located
  }

type bridge_expr =
  | Source_field of string Location.located
  | Morph_apply of
      { morph : string Location.located;
        field : string Location.located
      }
  | Min of Location.t
  | Max of Location.t
  | Join of
      { left : bridge_expr;
        right : bridge_expr;
        loc : Location.t
      }
  | Meet of
      { left : bridge_expr;
        right : bridge_expr;
        loc : Location.t
      }

type bridge_assignment =
  { target : string Location.located;
    expr : bridge_expr
  }

type adjoint_chain =
  { names : string Location.located list;
    loc : Location.t
  }

type decl =
  | Base of
      { name : string Location.located;
        clauses : chain list
      }
  | Product of
      { name : string Location.located;
        fields : field list
      }
  | Primitive_morph of
      { name : string Location.located;
        source : lattice_expr;
        target : lattice_expr;
        mappings : primitive_mapping list
      }
  | Product_bridge of
      { name : string Location.located;
        source : lattice_expr;
        target : lattice_expr;
        assignments : bridge_assignment list
      }
  | Adjoint_chain of adjoint_chain

type file = decl list
