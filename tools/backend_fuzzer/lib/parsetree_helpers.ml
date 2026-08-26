open Ast_helper
open Asttypes

let loc name = { Location.txt = name; loc = Location.none }

let lid name = loc (Longident.Lident name)

let ldot module_name name = loc (Longident.Ldot (lid module_name, loc name))

let ident name = Exp.ident (lid name)

let qualified_ident module_name name = Exp.ident (ldot module_name name)

let int n = Exp.constant (Const.int n)

let unit_ = Exp.construct (lid "()") None

let apply id args = Exp.apply id (List.map (fun a -> Nolabel, a) args)

let op name args = apply (ident name) args

let value_param pattern : Parsetree.function_param =
  { pparam_loc = Location.none;
    pparam_desc = Pparam_val (Nolabel, None, pattern)
  }

let function_ params body =
  Exp.function_ params
    { mode_annotations = [];
      ret_mode_annotations = [];
      ret_type_constraint = None
    }
    (Pfunction_body body)
