open Std
open Sexplib.Std
open Ocaml_ir_fiber
module Reason =
  struct
    type t =
      | Compiler_exn of string 
      | Ocaml_ir_comp_error of Error.t 
      | Build_system_file_missing of string * Error.t 
      | File_missing of string * Error.t option 
      | Not_in_repo of Error.t 
      | Unsupported_source_file_extension 
      | Cant_use_dot_merlin_file of string 
      | Unsupported_language of string 
      | Problem_finding_library of string 
      | Problem_finding_ocaml_ir of Error.t 
      | Other_error of Error.t 
      | No_ir_produced of Language.t [@@deriving sexp]
    include
      struct
        let _ = fun (_ : t) -> ()
        let t_of_sexp =
          (let error_source__003_ = "result.ml.Reason.t" in
           function
           | Sexplib0.Sexp.List ((Sexplib0.Sexp.Atom
               ("compiler_exn" | "Compiler_exn" as _tag__006_))::sexp_args__007_)
               as _sexp__005_ ->
               (match sexp_args__007_ with
                | arg0__008_::[] ->
                    let res0__009_ = string_of_sexp arg0__008_ in
                    Compiler_exn res0__009_
                | _ ->
                    Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                      error_source__003_ _tag__006_ _sexp__005_)
           | Sexplib0.Sexp.List ((Sexplib0.Sexp.Atom
               ("ocaml_ir_comp_error" | "Ocaml_ir_comp_error" as _tag__011_))::sexp_args__012_)
               as _sexp__010_ ->
               (match sexp_args__012_ with
                | arg0__013_::[] ->
                    let res0__014_ = Error.t_of_sexp arg0__013_ in
                    Ocaml_ir_comp_error res0__014_
                | _ ->
                    Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                      error_source__003_ _tag__011_ _sexp__010_)
           | Sexplib0.Sexp.List ((Sexplib0.Sexp.Atom
               ("build_system_file_missing" | "Build_system_file_missing" as
                  _tag__016_))::sexp_args__017_)
               as _sexp__015_ ->
               (match sexp_args__017_ with
                | arg0__018_::arg1__019_::[] ->
                    let res0__020_ = string_of_sexp arg0__018_
                    and res1__021_ = Error.t_of_sexp arg1__019_ in
                    Build_system_file_missing (res0__020_, res1__021_)
                | _ ->
                    Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                      error_source__003_ _tag__016_ _sexp__015_)
           | Sexplib0.Sexp.List ((Sexplib0.Sexp.Atom
               ("file_missing" | "File_missing" as _tag__023_))::sexp_args__024_)
               as _sexp__022_ ->
               (match sexp_args__024_ with
                | arg0__025_::arg1__026_::[] ->
                    let res0__027_ = string_of_sexp arg0__025_
                    and res1__028_ =
                      option_of_sexp Error.t_of_sexp arg1__026_ in
                    File_missing (res0__027_, res1__028_)
                | _ ->
                    Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                      error_source__003_ _tag__023_ _sexp__022_)
           | Sexplib0.Sexp.List ((Sexplib0.Sexp.Atom
               ("not_in_repo" | "Not_in_repo" as _tag__030_))::sexp_args__031_)
               as _sexp__029_ ->
               (match sexp_args__031_ with
                | arg0__032_::[] ->
                    let res0__033_ = Error.t_of_sexp arg0__032_ in
                    Not_in_repo res0__033_
                | _ ->
                    Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                      error_source__003_ _tag__030_ _sexp__029_)
           | Sexplib0.Sexp.Atom
               ("unsupported_source_file_extension"
                | "Unsupported_source_file_extension")
               -> Unsupported_source_file_extension
           | Sexplib0.Sexp.List ((Sexplib0.Sexp.Atom
               ("cant_use_dot_merlin_file" | "Cant_use_dot_merlin_file" as
                  _tag__035_))::sexp_args__036_)
               as _sexp__034_ ->
               (match sexp_args__036_ with
                | arg0__037_::[] ->
                    let res0__038_ = string_of_sexp arg0__037_ in
                    Cant_use_dot_merlin_file res0__038_
                | _ ->
                    Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                      error_source__003_ _tag__035_ _sexp__034_)
           | Sexplib0.Sexp.List ((Sexplib0.Sexp.Atom
               ("unsupported_language" | "Unsupported_language" as _tag__040_))::sexp_args__041_)
               as _sexp__039_ ->
               (match sexp_args__041_ with
                | arg0__042_::[] ->
                    let res0__043_ = string_of_sexp arg0__042_ in
                    Unsupported_language res0__043_
                | _ ->
                    Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                      error_source__003_ _tag__040_ _sexp__039_)
           | Sexplib0.Sexp.List ((Sexplib0.Sexp.Atom
               ("problem_finding_library" | "Problem_finding_library" as
                  _tag__045_))::sexp_args__046_)
               as _sexp__044_ ->
               (match sexp_args__046_ with
                | arg0__047_::[] ->
                    let res0__048_ = string_of_sexp arg0__047_ in
                    Problem_finding_library res0__048_
                | _ ->
                    Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                      error_source__003_ _tag__045_ _sexp__044_)
           | Sexplib0.Sexp.List ((Sexplib0.Sexp.Atom
               ("problem_finding_ocaml_ir" | "Problem_finding_ocaml_ir" as
                  _tag__050_))::sexp_args__051_)
               as _sexp__049_ ->
               (match sexp_args__051_ with
                | arg0__052_::[] ->
                    let res0__053_ = Error.t_of_sexp arg0__052_ in
                    Problem_finding_ocaml_ir res0__053_
                | _ ->
                    Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                      error_source__003_ _tag__050_ _sexp__049_)
           | Sexplib0.Sexp.List ((Sexplib0.Sexp.Atom
               ("other_error" | "Other_error" as _tag__055_))::sexp_args__056_)
               as _sexp__054_ ->
               (match sexp_args__056_ with
                | arg0__057_::[] ->
                    let res0__058_ = Error.t_of_sexp arg0__057_ in
                    Other_error res0__058_
                | _ ->
                    Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                      error_source__003_ _tag__055_ _sexp__054_)
           | Sexplib0.Sexp.List ((Sexplib0.Sexp.Atom
               ("no_ir_produced" | "No_ir_produced" as _tag__060_))::sexp_args__061_)
               as _sexp__059_ ->
               (match sexp_args__061_ with
                | arg0__062_::[] ->
                    let res0__063_ = Language.t_of_sexp arg0__062_ in
                    No_ir_produced res0__063_
                | _ ->
                    Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                      error_source__003_ _tag__060_ _sexp__059_)
           | Sexplib0.Sexp.List ((Sexplib0.Sexp.Atom
               ("unsupported_source_file_extension"
                | "Unsupported_source_file_extension"))::_)
               as sexp__004_ ->
               Sexplib0.Sexp_conv_error.stag_no_args error_source__003_
                 sexp__004_
           | Sexplib0.Sexp.Atom
               ("compiler_exn" | "Compiler_exn" | "ocaml_ir_comp_error"
                | "Ocaml_ir_comp_error" | "build_system_file_missing"
                | "Build_system_file_missing" | "file_missing"
                | "File_missing" | "not_in_repo" | "Not_in_repo"
                | "cant_use_dot_merlin_file" | "Cant_use_dot_merlin_file"
                | "unsupported_language" | "Unsupported_language"
                | "problem_finding_library" | "Problem_finding_library"
                | "problem_finding_ocaml_ir" | "Problem_finding_ocaml_ir"
                | "other_error" | "Other_error" | "no_ir_produced"
                | "No_ir_produced")
               as sexp__004_ ->
               Sexplib0.Sexp_conv_error.stag_takes_args error_source__003_
                 sexp__004_
           | Sexplib0.Sexp.List ((Sexplib0.Sexp.List _)::_) as sexp__002_ ->
               Sexplib0.Sexp_conv_error.nested_list_invalid_sum
                 error_source__003_ sexp__002_
           | Sexplib0.Sexp.List [] as sexp__002_ ->
               Sexplib0.Sexp_conv_error.empty_list_invalid_sum
                 error_source__003_ sexp__002_
           | sexp__002_ ->
               Sexplib0.Sexp_conv_error.unexpected_stag error_source__003_
                 ["Compiler_exn";
                 "Ocaml_ir_comp_error";
                 "Build_system_file_missing";
                 "File_missing";
                 "Not_in_repo";
                 "Unsupported_source_file_extension";
                 "Cant_use_dot_merlin_file";
                 "Unsupported_language";
                 "Problem_finding_library";
                 "Problem_finding_ocaml_ir";
                 "Other_error";
                 "No_ir_produced"] sexp__002_ : Sexplib0.Sexp.t -> t)
        let _ = t_of_sexp
        let sexp_of_t =
          (function
           | Compiler_exn arg0__064_ ->
               let res0__065_ = sexp_of_string arg0__064_ in
               Sexplib0.Sexp.List
                 [Sexplib0.Sexp.Atom "Compiler_exn"; res0__065_]
           | Ocaml_ir_comp_error arg0__066_ ->
               let res0__067_ = Error.sexp_of_t arg0__066_ in
               Sexplib0.Sexp.List
                 [Sexplib0.Sexp.Atom "Ocaml_ir_comp_error"; res0__067_]
           | Build_system_file_missing (arg0__068_, arg1__069_) ->
               let res0__070_ = sexp_of_string arg0__068_
               and res1__071_ = Error.sexp_of_t arg1__069_ in
               Sexplib0.Sexp.List
                 [Sexplib0.Sexp.Atom "Build_system_file_missing";
                 res0__070_;
                 res1__071_]
           | File_missing (arg0__072_, arg1__073_) ->
               let res0__074_ = sexp_of_string arg0__072_
               and res1__075_ = sexp_of_option Error.sexp_of_t arg1__073_ in
               Sexplib0.Sexp.List
                 [Sexplib0.Sexp.Atom "File_missing"; res0__074_; res1__075_]
           | Not_in_repo arg0__076_ ->
               let res0__077_ = Error.sexp_of_t arg0__076_ in
               Sexplib0.Sexp.List
                 [Sexplib0.Sexp.Atom "Not_in_repo"; res0__077_]
           | Unsupported_source_file_extension ->
               Sexplib0.Sexp.Atom "Unsupported_source_file_extension"
           | Cant_use_dot_merlin_file arg0__078_ ->
               let res0__079_ = sexp_of_string arg0__078_ in
               Sexplib0.Sexp.List
                 [Sexplib0.Sexp.Atom "Cant_use_dot_merlin_file"; res0__079_]
           | Unsupported_language arg0__080_ ->
               let res0__081_ = sexp_of_string arg0__080_ in
               Sexplib0.Sexp.List
                 [Sexplib0.Sexp.Atom "Unsupported_language"; res0__081_]
           | Problem_finding_library arg0__082_ ->
               let res0__083_ = sexp_of_string arg0__082_ in
               Sexplib0.Sexp.List
                 [Sexplib0.Sexp.Atom "Problem_finding_library"; res0__083_]
           | Problem_finding_ocaml_ir arg0__084_ ->
               let res0__085_ = Error.sexp_of_t arg0__084_ in
               Sexplib0.Sexp.List
                 [Sexplib0.Sexp.Atom "Problem_finding_ocaml_ir"; res0__085_]
           | Other_error arg0__086_ ->
               let res0__087_ = Error.sexp_of_t arg0__086_ in
               Sexplib0.Sexp.List
                 [Sexplib0.Sexp.Atom "Other_error"; res0__087_]
           | No_ir_produced arg0__088_ ->
               let res0__089_ = Language.sexp_of_t arg0__088_ in
               Sexplib0.Sexp.List
                 [Sexplib0.Sexp.Atom "No_ir_produced"; res0__089_] : 
          t -> Sexplib0.Sexp.t)
        let _ = sexp_of_t
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    let to_string =
      function
      | Compiler_exn str -> Printf.sprintf "Compilation error: %s" str
      | Ocaml_ir_comp_error err ->
          Printf.sprintf "Error invoking ocaml-ir-comp: %s"
            (Error.to_string_hum err)
      | Build_system_file_missing (file, err) ->
          Printf.sprintf
            "Is the target built and up to date? File %s is missing. (%s)"
            file (Error.to_string_hum err)
      | File_missing (file, None) ->
          Printf.sprintf "File %s must exist and be readable" file
      | File_missing (file, Some err) ->
          Printf.sprintf "File %s must exist and be readable: %s" file
            (Error.to_string_hum err)
      | Not_in_repo err ->
          Printf.sprintf "Source file is not found to be in a repository: %s"
            (Error.to_string_hum err)
      | Unsupported_source_file_extension ->
          "The source file must have .ml or .mli extension"
      | Cant_use_dot_merlin_file s ->
          Printf.sprintf "Problem with .merlin: %s" s
      | Unsupported_language s -> Printf.sprintf "Unsupported language: %s" s
      | Problem_finding_library s ->
          Printf.sprintf "Problem finding library: %s" s
      | Problem_finding_ocaml_ir err ->
          Printf.sprintf "Error finding ocaml-ir-comp: %s"
            (Error.to_string_hum err)
      | Other_error err -> Error.to_string_hum err
      | No_ir_produced language ->
          Printf.sprintf
            "No ir was produced while compiling this file for language %s"
            (Language.string_of_t language)
  end
type 'a t =
  | Ok of 'a 
  | Error of Reason.t [@@deriving sexp]
include
  struct
    let _ = fun (_ : 'a t) -> ()
    let t_of_sexp : 'a . (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a t =
      fun (type a__105_) ->
        (let error_source__093_ = "result.ml.t" in
         fun _of_a__090_ ->
           function
           | Sexplib0.Sexp.List ((Sexplib0.Sexp.Atom
               ("ok" | "Ok" as _tag__096_))::sexp_args__097_) as _sexp__095_
               ->
               (match sexp_args__097_ with
                | arg0__098_::[] ->
                    let res0__099_ = _of_a__090_ arg0__098_ in Ok res0__099_
                | _ ->
                    Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                      error_source__093_ _tag__096_ _sexp__095_)
           | Sexplib0.Sexp.List ((Sexplib0.Sexp.Atom
               ("error" | "Error" as _tag__101_))::sexp_args__102_) as
               _sexp__100_ ->
               (match sexp_args__102_ with
                | arg0__103_::[] ->
                    let res0__104_ = Reason.t_of_sexp arg0__103_ in
                    Error res0__104_
                | _ ->
                    Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                      error_source__093_ _tag__101_ _sexp__100_)
           | Sexplib0.Sexp.Atom ("ok" | "Ok" | "error" | "Error") as
               sexp__094_ ->
               Sexplib0.Sexp_conv_error.stag_takes_args error_source__093_
                 sexp__094_
           | Sexplib0.Sexp.List ((Sexplib0.Sexp.List _)::_) as sexp__092_ ->
               Sexplib0.Sexp_conv_error.nested_list_invalid_sum
                 error_source__093_ sexp__092_
           | Sexplib0.Sexp.List [] as sexp__092_ ->
               Sexplib0.Sexp_conv_error.empty_list_invalid_sum
                 error_source__093_ sexp__092_
           | sexp__092_ ->
               Sexplib0.Sexp_conv_error.unexpected_stag error_source__093_
                 ["Ok"; "Error"] sexp__092_ : (Sexplib0.Sexp.t -> a__105_) ->
                                                Sexplib0.Sexp.t -> a__105_ t)
    let _ = t_of_sexp
    let sexp_of_t : 'a . ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
      fun (type a__111_) ->
        (fun _of_a__106_ ->
           function
           | Ok arg0__107_ ->
               let res0__108_ = _of_a__106_ arg0__107_ in
               Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Ok"; res0__108_]
           | Error arg0__109_ ->
               let res0__110_ = Reason.sexp_of_t arg0__109_ in
               Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Error"; res0__110_] : 
        (a__111_ -> Sexplib0.Sexp.t) -> a__111_ t -> Sexplib0.Sexp.t)
    let _ = sexp_of_t
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
module Monad_arg =
  struct
    type nonrec 'a t = 'a t
    let return x = Ok x
    let map o ~f = match o with | Ok x -> Ok (f x) | Error n -> Error n
    let bind o ~f = match o with | Ok x -> f x | Error n -> Error n
  end
include (Monad.Make)(Monad_arg)
let error reason = Error reason
let compiler_exn m = Error (Compiler_exn m)
let ocaml_ir_comp_error err = Error (Ocaml_ir_comp_error err)
let build_system_file_missing ~file err =
  Error (Build_system_file_missing (file, err))
let file_missing ?err ~file () = Error (File_missing (file, err))
let not_in_repo err = Error (Not_in_repo err)
let unsupported_source_file_extension =
  Error Unsupported_source_file_extension
let cant_use_dot_merlin_file m = Error (Cant_use_dot_merlin_file m)
let unsupported_language m = Error (Unsupported_language m)
let problem_finding_library m = Error (Problem_finding_library m)
let problem_finding_ocaml_ir err = Error (Problem_finding_ocaml_ir err)
let other_error err = Error (Other_error err)
let no_ir_produced language = Error (No_ir_produced language)
let ok_exn =
  function
  | Ok x -> x
  | Error reason ->
      Error.raise (Error.createf "%s" (Reason.to_string reason))
let bind_to_deferred o ~f =
  match o with | Ok x -> f x | Error n -> (Error n) |> Fiber.return
let value_ignore_error ~default = function | Ok x -> x | Error _ -> default
module Fiber =
  struct
    module Monad_arg =
      struct
        type nonrec 'a t = 'a t Fiber.t
        let return x = Fiber.return (Ok x)
        let map o ~f = Fiber.map o ~f:(map ~f)
        let bind o ~f =
          Fiber.bind o
            ~f:(fun o ->
                  match o with
                  | Ok x -> f x
                  | Error n -> (Error n) |> Fiber.return)
      end
    include (Monad.Make)(Monad_arg)
    type nonrec 'a t = 'a t Fiber.t
    let ok t = Fiber.map t ~f:(fun v -> Ok v)
  end
