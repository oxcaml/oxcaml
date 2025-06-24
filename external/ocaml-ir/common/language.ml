open Std
module T =
  struct
    type t =
      | Assembly
      | Cfg
      | Cmm
      | Flambda2
      | Lambda
      | Linear
      | Parse_tree
      | Ppx
      | Raw_flambda2
      | Raw_lambda
      | Typed_tree
      | Inlining_report
      | Check_allocations [@@deriving
                            (equal, compare, enumerate, sexp, hash)]
    include
      struct
        let _ = fun (_ : t) -> ()
        let equal = (Stdlib.(=) : t -> ((t)[@merlin.hide ]) -> bool)
        let _ = equal
        let compare = (Stdlib.compare : t -> ((t)[@merlin.hide ]) -> int)
        let _ = compare
        let all =
          ([Assembly;
           Cfg;
           Cmm;
           Flambda2;
           Lambda;
           Linear;
           Parse_tree;
           Ppx;
           Raw_flambda2;
           Raw_lambda;
           Typed_tree;
           Inlining_report;
           Check_allocations] : t list)
        let _ = all
        let t_of_sexp =
          (let error_source__007_ = "language.ml.T.t" in
           function
           | Sexplib0.Sexp.Atom ("assembly" | "Assembly") -> Assembly
           | Sexplib0.Sexp.Atom ("cfg" | "Cfg") -> Cfg
           | Sexplib0.Sexp.Atom ("cmm" | "Cmm") -> Cmm
           | Sexplib0.Sexp.Atom ("flambda2" | "Flambda2") -> Flambda2
           | Sexplib0.Sexp.Atom ("lambda" | "Lambda") -> Lambda
           | Sexplib0.Sexp.Atom ("linear" | "Linear") -> Linear
           | Sexplib0.Sexp.Atom ("parse_tree" | "Parse_tree") -> Parse_tree
           | Sexplib0.Sexp.Atom ("ppx" | "Ppx") -> Ppx
           | Sexplib0.Sexp.Atom ("raw_flambda2" | "Raw_flambda2") ->
               Raw_flambda2
           | Sexplib0.Sexp.Atom ("raw_lambda" | "Raw_lambda") -> Raw_lambda
           | Sexplib0.Sexp.Atom ("typed_tree" | "Typed_tree") -> Typed_tree
           | Sexplib0.Sexp.Atom ("inlining_report" | "Inlining_report") ->
               Inlining_report
           | Sexplib0.Sexp.Atom ("check_allocations" | "Check_allocations")
               -> Check_allocations
           | Sexplib0.Sexp.List ((Sexplib0.Sexp.Atom
               ("assembly" | "Assembly" | "cfg" | "Cfg" | "cmm" | "Cmm"
                | "flambda2" | "Flambda2" | "lambda" | "Lambda" | "linear"
                | "Linear" | "parse_tree" | "Parse_tree" | "ppx" | "Ppx"
                | "raw_flambda2" | "Raw_flambda2" | "raw_lambda"
                | "Raw_lambda" | "typed_tree" | "Typed_tree"
                | "inlining_report" | "Inlining_report" | "check_allocations"
                | "Check_allocations"))::_)
               as sexp__008_ ->
               Sexplib0.Sexp_conv_error.stag_no_args error_source__007_
                 sexp__008_
           | Sexplib0.Sexp.List ((Sexplib0.Sexp.List _)::_) as sexp__006_ ->
               Sexplib0.Sexp_conv_error.nested_list_invalid_sum
                 error_source__007_ sexp__006_
           | Sexplib0.Sexp.List [] as sexp__006_ ->
               Sexplib0.Sexp_conv_error.empty_list_invalid_sum
                 error_source__007_ sexp__006_
           | sexp__006_ ->
               Sexplib0.Sexp_conv_error.unexpected_stag error_source__007_
                 ["Assembly";
                 "Cfg";
                 "Cmm";
                 "Flambda2";
                 "Lambda";
                 "Linear";
                 "Parse_tree";
                 "Ppx";
                 "Raw_flambda2";
                 "Raw_lambda";
                 "Typed_tree";
                 "Inlining_report";
                 "Check_allocations"] sexp__006_ : Sexplib0.Sexp.t -> t)
        let _ = t_of_sexp
        let sexp_of_t =
          (function
           | Assembly -> Sexplib0.Sexp.Atom "Assembly"
           | Cfg -> Sexplib0.Sexp.Atom "Cfg"
           | Cmm -> Sexplib0.Sexp.Atom "Cmm"
           | Flambda2 -> Sexplib0.Sexp.Atom "Flambda2"
           | Lambda -> Sexplib0.Sexp.Atom "Lambda"
           | Linear -> Sexplib0.Sexp.Atom "Linear"
           | Parse_tree -> Sexplib0.Sexp.Atom "Parse_tree"
           | Ppx -> Sexplib0.Sexp.Atom "Ppx"
           | Raw_flambda2 -> Sexplib0.Sexp.Atom "Raw_flambda2"
           | Raw_lambda -> Sexplib0.Sexp.Atom "Raw_lambda"
           | Typed_tree -> Sexplib0.Sexp.Atom "Typed_tree"
           | Inlining_report -> Sexplib0.Sexp.Atom "Inlining_report"
           | Check_allocations -> Sexplib0.Sexp.Atom "Check_allocations" :
          t -> Sexplib0.Sexp.t)
        let _ = sexp_of_t
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end
include T
module StringMap = (Map.Make)(String)
module Map =
  struct
    module M = (Map.Make)(T)
    include M
    include (Make_map_sexp)(M)
    let t_of_sexp data_of_sexp m = map_of_sexp t_of_sexp data_of_sexp m
    let sexp_of_t sexp_of_data m = sexp_of_map sexp_of_t sexp_of_data m
  end
module Set = (Set.Make)(T)
let mapping =
  function
  | Assembly -> ("assembly", (Some "a"))
  | Cfg -> ("cfg", (Some "g"))
  | Cmm -> ("cmm", (Some "c"))
  | Flambda2 -> ("flambda2", (Some "f 2"))
  | Lambda -> ("lambda", (Some "l"))
  | Linear -> ("linear", (Some "n"))
  | Parse_tree -> ("parse-tree", (Some "p"))
  | Ppx -> ("ppx", (Some "x"))
  | Raw_flambda2 -> ("raw-flambda2", (Some "F 2"))
  | Raw_lambda -> ("raw-lambda", (Some "L"))
  | Typed_tree -> ("typed-tree", (Some "t"))
  | Inlining_report -> ("inlining-report", None)
  | Check_allocations -> ("check-allocations", None)
let t_of_string s =
  let map =
    ((all |> (List.map ~f:(fun lang -> ((fst (mapping lang)), lang)))) |>
       List.to_seq)
      |> StringMap.of_seq in
  StringMap.find_opt s map
let t_of_string_exn language =
  (t_of_string language) |>
    (Option.value_exn
       ~here:{
               Lexing.pos_fname = "lib/ocaml_ir/common/language.ml";
               pos_lnum = 65;
               pos_cnum = 1471;
               pos_bol = 1458
             }
       ~message:("Unsupported intermediate representation: " ^ language))
let string_of_t t = (mapping t) |> fst
let unique_letter_binding t = (mapping t) |> snd
let camel_case_of_t =
  let rem_dashes s =
    (((String.split_on_char ~sep:'-' s) |>
        (List.filter ~f:(fun s -> (String.length s) > 0)))
       |> (List.map ~f:String.capitalize_ascii))
      |> (String.concat ~sep:"") in
  fun t -> (string_of_t t) |> rem_dashes
let first () = Ppx
let next =
  function
  | Ppx -> [Parse_tree]
  | Parse_tree -> [Typed_tree]
  | Typed_tree -> [Raw_lambda]
  | Raw_lambda -> [Lambda]
  | Lambda -> [Raw_flambda2]
  | Raw_flambda2 -> [Flambda2]
  | Flambda2 -> [Cmm]
  | Cmm -> [Cfg]
  | Cfg -> [Linear]
  | Linear -> [Assembly]
  | Assembly -> []
  | Inlining_report -> []
  | Check_allocations -> []
