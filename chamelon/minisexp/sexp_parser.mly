(******************************************************************************
 *                                 Sexplib                                    *
 *                                                                            *
 * Copyright (c) 2005--2026 Jane Street Group, LLC                            *
 *   <opensource-contacts@janestreet.com>                                     *
 ******************************************************************************)

(* This file has been adapted from
   https://github.com/janestreet/sexplib/blob/16ed74eb78c67e76e6cf9219c1b06b93b8c01c53/src/parser.mly
 *)

%token <string> STRING
%token LPAREN RPAREN EOF HASH_SEMI

%start sexp
%type <Sexp.t> sexp

%start sexp_opt
%type <Sexp.t option> sexp_opt

%start sexps
%type <Sexp.t list> sexps

%start rev_sexps
%type <Sexp.t list> rev_sexps

%%

sexp:
| sexp_comments sexp_but_no_comment { $2 }
| sexp_but_no_comment { $1 }

sexp_but_no_comment
  : STRING { Sexp.Atom $1 }
  | LPAREN RPAREN { Sexp.List [] }
  | LPAREN rev_sexps_aux RPAREN { Sexp.List (List.rev $2) }

sexp_comment
  : HASH_SEMI sexp_but_no_comment { () }
  | HASH_SEMI sexp_comments sexp_but_no_comment { () }

sexp_comments
  : sexp_comment { () }
  | sexp_comments sexp_comment { () }

sexp_opt
  : sexp_but_no_comment { Some $1 }
  | sexp_comments sexp_but_no_comment { Some $2 }
  | EOF { None }
  | sexp_comments EOF { None }

rev_sexps_aux
  : sexp_but_no_comment { [$1] }
  | sexp_comment { [] }
  | rev_sexps_aux sexp_but_no_comment { $2 :: $1 }
  | rev_sexps_aux sexp_comment { $1 }

rev_sexps
  : rev_sexps_aux EOF { $1 }
  | EOF { [] }

sexps
  : rev_sexps_aux EOF { List.rev $1 }
  | EOF { [] }
