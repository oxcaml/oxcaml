type error =
  | Lexing_error of Flambda_lex.error * Location.t
  | Parsing_error of string * Location.t

val parse_expect_test_spec : string -> (Fexpr.expect_test_spec, error) result

val parse_fexpr : string -> (Fexpr.flambda_unit, error) result

val parse_markdown_doc : string -> (Fexpr.markdown_doc, error) result

val make_unit_info : filename:string -> Unit_info.t

val parse : string -> (Flambda_unit.t, error) result
