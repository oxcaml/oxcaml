external select
  : 'a.
  bool -> ('a[@local_opt]) -> ('a[@local_opt]) -> ('a[@local_opt])
  = "caml_csel_value"
[@@noalloc]
[@@no_effects]
[@@no_coeffects]
[@@builtin]

external select_int64
  :  bool
  -> (int64[@unboxed])
  -> (int64[@unboxed])
  -> (int64[@unboxed])
  @@ portable
  = "caml_csel_value" "caml_csel_int64_unboxed"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]
