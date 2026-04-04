%{
open Ast

let merge_names
    (first : string Location.located)
    (rest : string Location.located list)
  =
  match List.rev rest with
  | [] -> first.loc
  | last :: _ -> Location.merge first.loc last.loc
%}

%token <string Location.located> IDENT
%token LBRACKET RBRACKET LBRACE RBRACE
%token EQ LT GT LEQ ARROW CARET COLON SEMI
%token VIA ALIASES
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
  | small = IDENT LEQ big = IDENT VIA LBRACE mappings = mapping_list RBRACE aliases = aliases_opt
      { Embedding { small; big; mappings; aliases } }

aliases_opt:
  | { [] }
  | ALIASES LBRACE aliases = alias_list RBRACE { aliases }

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
  | first = mapping rest = mapping_tail_list { first :: rest }

mapping_tail_list:
  | { [] }
  | SEMI { [] }
  | SEMI next = mapping rest = mapping_tail_list { next :: rest }

alias_list:
  | { [] }
  | first = alias_decl rest = alias_tail_list { first :: rest }

alias_tail_list:
  | { [] }
  | SEMI { [] }
  | SEMI next = alias_decl rest = alias_tail_list { next :: rest }

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

field:
  | name = IDENT COLON lattice = IDENT opposite = field_opposite
      { { name; ty = { lattice; opposite } } }

field_opposite:
  | { false }
  | CARET kw = IDENT
      {
        let kw : string Location.located = kw in
        if kw.txt <> "op"
        then Error.failf kw.loc "expected \"op\" after '^'";
        true
      }

mapping:
  | small = IDENT ARROW big = IDENT { { small; big } }

alias_decl:
  | slot = IDENT EQ name = IDENT { { slot; name } }
