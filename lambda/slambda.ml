type error = Unsupported of string

exception Error of Location.t option * error

type program =
  { compilation_unit : Compilation_unit.t;
    main_module_block_format : Lambda.main_module_block_format;
    arg_block_idx : int option;
    code : Lambda.slambda
  }

(* Error reporting *)
open Format

let raise ?loc err = raise (Error (loc, err))

let report_error ppf = function
  | Unsupported where ->
    fprintf ppf
      "Static computation and layout polymorphism are not yet supported in %s."
      where

let () =
  Location.register_error_of_exn (function
    | Error (Some loc, err) ->
      Some (Location.error_of_printer ~loc report_error err)
    | Error (None, err) -> Some (Location.error_of_printer report_error err)
    | _ -> None)
