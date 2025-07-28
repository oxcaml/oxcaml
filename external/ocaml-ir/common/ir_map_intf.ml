open Sexplib.Std

module Label = struct
  type t =
    | Function of string
    | Expr of string option
  [@@deriving sexp]

  include struct
    let _ = fun (_ : t) -> ()

    let t_of_sexp =
      (let error_source__003_ = "ir_map_intf.ml.Label.t" in
       function
       | Sexplib0.Sexp.List
           (Sexplib0.Sexp.Atom (("function" | "Function") as _tag__006_)
           :: sexp_args__007_) as _sexp__005_ ->
         (match sexp_args__007_ with
          | arg0__008_ :: [] ->
            let res0__009_ = string_of_sexp arg0__008_ in
            Function res0__009_
          | _ ->
            Sexplib0.Sexp_conv_error.stag_incorrect_n_args
              error_source__003_
              _tag__006_
              _sexp__005_)
       | Sexplib0.Sexp.List
           (Sexplib0.Sexp.Atom (("expr" | "Expr") as _tag__011_) :: sexp_args__012_) as
         _sexp__010_ ->
         (match sexp_args__012_ with
          | arg0__013_ :: [] ->
            let res0__014_ = option_of_sexp string_of_sexp arg0__013_ in
            Expr res0__014_
          | _ ->
            Sexplib0.Sexp_conv_error.stag_incorrect_n_args
              error_source__003_
              _tag__011_
              _sexp__010_)
       | Sexplib0.Sexp.Atom ("function" | "Function" | "expr" | "Expr") as sexp__004_ ->
         Sexplib0.Sexp_conv_error.stag_takes_args error_source__003_ sexp__004_
       | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__002_ ->
         Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__003_ sexp__002_
       | Sexplib0.Sexp.List [] as sexp__002_ ->
         Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__003_ sexp__002_
       | sexp__002_ ->
         Sexplib0.Sexp_conv_error.unexpected_stag
           error_source__003_
           [ "Function"; "Expr" ]
           sexp__002_
        : Sexplib0.Sexp.t -> t)
    ;;

    let _ = t_of_sexp

    let sexp_of_t =
      (function
       | Function arg0__015_ ->
         let res0__016_ = sexp_of_string arg0__015_ in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Function"; res0__016_ ]
       | Expr arg0__017_ ->
         let res0__018_ = sexp_of_option sexp_of_string arg0__017_ in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Expr"; res0__018_ ]
        : t -> Sexplib0.Sexp.t)
    ;;

    let _ = sexp_of_t
  end [@@ocaml.doc "@inline"] [@@merlin.hide]
end
