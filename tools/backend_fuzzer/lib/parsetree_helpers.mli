val loc : 'a -> 'a Location.loc
val lid : string -> Longident.t Location.loc
val ldot : string -> string -> Longident.t Location.loc
val ident : string -> Parsetree.expression
val qualified_ident : string -> string -> Parsetree.expression
val int : int -> Parsetree.expression
val unit_ : Parsetree.expression
val apply :
  Parsetree.expression -> Parsetree.expression list -> Parsetree.expression
val op : string -> Parsetree.expression list -> Parsetree.expression
val value_param : Parsetree.pattern -> Parsetree.function_param
val function_ :
  Parsetree.function_param list ->
  Parsetree.expression ->
  Parsetree.expression
