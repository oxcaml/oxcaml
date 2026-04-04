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

type field_ty =
  { lattice : string Location.located;
    opposite : bool
  }

type field =
  { name : string Location.located;
    ty : field_ty
  }

type mapping =
  { small : string Location.located;
    big : string Location.located
  }

type alias =
  { slot : string Location.located;
    name : string Location.located
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
  | Embedding of
      { small : string Location.located;
        big : string Location.located;
        mappings : mapping list;
        aliases : alias list
      }

type file = decl list
