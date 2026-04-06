%{
open Ast

let merge_names
    (first : string Location.located)
    (rest : string Location.located list)
  =
  match List.rev rest with
  | [] -> first.Location.loc
  | last :: _ -> Location.merge first.Location.loc last.Location.loc

let bridge_expr_loc = function
  | Source_field field -> field.Location.loc
  | Morph_apply { morph; field } -> Location.merge morph.Location.loc field.Location.loc
  | Min loc | Max loc -> loc
  | Join { loc; _ } | Meet { loc; _ } -> loc
%}

%token <string Location.located> IDENT
%token LBRACKET RBRACKET LBRACE RBRACE
%token LPAREN RPAREN COMMA
%token EQ LT GT ARROW CARET COLON SEMI
%token EOF

%start <Ast.file> file

%%

file:
  | decls = decls EOF { decls }

decls:
  | { [] }
  | decl = decl rest = decls { decl :: rest }

decl:
  | name = IDENT EQ LBRACKET clauses = clause_list RBRACKET
      { Base { name; clauses } }
  | name = IDENT EQ LBRACE fields = field_list RBRACE
      { Product { name; fields } }
  | name = IDENT COLON source = lattice_expr ARROW target = lattice_expr EQ LBRACKET mappings = mapping_list RBRACKET
      { Primitive_morph { name; source; target; mappings } }
  | name = IDENT COLON source = lattice_expr ARROW target = lattice_expr EQ LBRACE assignments = bridge_assignment_list RBRACE
      { Product_bridge { name; source; target; assignments } }

clause_list:
  | { [] }
  | first = clause rest = clause_tail_list { first :: rest }

clause_tail_list:
  | { [] }
  | SEMI { [] }
  | SEMI next = clause rest = clause_tail_list { next :: rest }

field_list:
  | { [] }
  | first = field rest = field_tail_list { first :: rest }

field_tail_list:
  | { [] }
  | SEMI { [] }
  | SEMI next = field rest = field_tail_list { next :: rest }

mapping_list:
  | { [] }
  | first = primitive_mapping rest = mapping_tail_list { first :: rest }

mapping_tail_list:
  | { [] }
  | SEMI { [] }
  | SEMI next = primitive_mapping rest = mapping_tail_list { next :: rest }

bridge_assignment_list:
  | { [] }
  | first = bridge_assignment rest = bridge_assignment_tail_list { first :: rest }

bridge_assignment_tail_list:
  | { [] }
  | SEMI { [] }
  | SEMI next = bridge_assignment rest = bridge_assignment_tail_list { next :: rest }

clause:
  | name = IDENT { Singleton name }
  | first = IDENT LT rest = lt_tail
      { Chain { direction = Lt; names = first :: rest; loc = merge_names first rest } }
  | first = IDENT GT rest = gt_tail
      { Chain { direction = Gt; names = first :: rest; loc = merge_names first rest } }

lt_tail:
  | name = IDENT { [name] }
  | name = IDENT LT rest = lt_tail { name :: rest }

gt_tail:
  | name = IDENT { [name] }
  | name = IDENT GT rest = gt_tail { name :: rest }

lattice_expr:
  | lattice = IDENT opposite = field_opposite
      { { lattice; opposite } }

field:
  | name = IDENT COLON ty = lattice_expr
      { { name; ty } }

field_opposite:
  | { false }
  | CARET kw = IDENT
      {
        let kw : string Location.located = kw in
        if kw.Location.txt <> "op"
        then Error.failf kw.Location.loc "expected \"op\" after '^'";
        true
      }

primitive_mapping:
  | source = IDENT ARROW target = IDENT { { source; target } }

bridge_assignment:
  | target = IDENT EQ expr = bridge_expr { { target; expr } }

bridge_expr:
  | kw = IDENT LPAREN left = bridge_expr COMMA right = bridge_expr RPAREN
      {
        if kw.Location.txt = "join"
        then Join { left; right; loc = Location.merge kw.Location.loc (bridge_expr_loc right) }
        else if kw.Location.txt = "meet"
        then Meet { left; right; loc = Location.merge kw.Location.loc (bridge_expr_loc right) }
        else
          Error.failf
            kw.Location.loc
            "unknown bridge expression operator %S (expected join or meet)"
            kw.Location.txt
      }
  | field = IDENT
      {
        if field.Location.txt = "min"
        then Min field.Location.loc
        else if field.Location.txt = "max"
        then Max field.Location.loc
        else Source_field field
      }
  | morph = IDENT LPAREN field = IDENT RPAREN
      { Morph_apply { morph; field } }
