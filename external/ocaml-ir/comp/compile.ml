module Ocaml_ir_location = Ocaml_ir_common.Location
module Ir_map = Ocaml_ir_common.Ir_map
module Ir_with_mappings = Ocaml_ir_common.Ir_with_mappings
module Language = Ocaml_ir_common.Language
module Result = Ocaml_ir_common.Result
open Ocaml_ir_fiber
open Ocaml_ir_common.Std
open Ocaml_ir_common.Result.Fiber.Let_syntax
open Sexplib.Std
module Ltf = Location_tracker_formatter

let ir_map_of_mappings ~filename (mappings : Ltf.Mappings.t) : Ir_map.raw =
  let basename = Filename.basename filename in
  mappings
  |> List.filter_map ~f:(fun item ->
       let source_location =
         Ocaml_ir_location.Simple.of_warnings_loc (Ltf.Mappings.Item.source item)
       in
       if String.equal
            (Filename.basename (Ocaml_ir_location.Simple.fname source_location))
            basename
       then (
         let ir_location =
           Ocaml_ir_location.Simple.of_warnings_loc (Ltf.Mappings.Item.ir item)
         in
         let label = Ir_map.Label.Expr None in
         Some (Ir_map.Entry.create ~label ~source_location ~ir_location))
       else None)
;;

module Printers = struct
  type item = T : ('a Compiler_hooks.pass * (Format.formatter -> 'a -> unit)) -> item

  let ( ++ ) t ((name : Language.t), hook, f) =
    Language.Map.update
      name
      (function
       | Some l -> Some (T (hook, f) :: l)
       | None -> Some [ T (hook, f) ])
      t
  ;;

  let print_implementation ppf typedtree =
    Printtyped.implementation ppf typedtree.Typedtree.structure
  ;;

  let hooks =
    let open Compiler_hooks in
    Language.Map.empty
    ++ (Cfg, Cfg, Cfg_with_layout.dump ~msg:"")
    ++ (Cmm, Cmm, fun fmt -> List.iter ~f:(Printcmm.phrase fmt))
    ++ (Flambda2, Flambda2, Flambda2_terms.Flambda_unit.print)
    ++ (Lambda, Lambda, Printlambda.program)
    ++ (Linear, Linear, Printlinear.fundecl)
    ++ (Raw_flambda2, Raw_flambda2, Flambda2_terms.Flambda_unit.print)
    ++ (Raw_lambda, Raw_lambda, Printlambda.program)
    ++ (Typed_tree, Typed_tree_intf, Printtyped.interface)
    ++ (Parse_tree, Parse_tree_intf, Printast_with_mappings.interface)
    ++ (Typed_tree, Typed_tree_impl, print_implementation)
    ++ (Parse_tree, Parse_tree_impl, Printast_with_mappings.implementation)
  ;;
end

let omit_dtimings_dprofile flags =
  List.filter
    ~f:
      (function
       | "-dtimings" | "-dgc-timings" | "-dprofile" -> false
       | _ -> true)
    flags
;;

let rec rewrite_output_prefix output_prefix args =
  match args with
  | [] -> []
  | "-o" :: _ :: tl -> "-o" :: output_prefix :: rewrite_output_prefix output_prefix tl
  | x :: tl -> x :: rewrite_output_prefix output_prefix tl
;;

let rec rewrite_filename ~filename args =
  match args with
  | [] -> []
  | (("-impl" | "-intf") as t) :: _ :: tl ->
    t :: filename :: rewrite_filename ~filename tl
  | x :: tl -> x :: rewrite_filename ~filename tl
;;

let adjust_compiler_flags ~filename ~compiler_flags ~languages ~output_prefix =
  let compiler_flags =
    if Language.Set.mem Assembly languages then "-S" :: compiler_flags else compiler_flags
  in
  rewrite_output_prefix output_prefix ("ocamlopt" :: "-g" :: "-c" :: compiler_flags)
  |> rewrite_filename ~filename
  |> omit_dtimings_dprofile
;;

let adjust_ocamlopt_env () =
  match Sys.getenv_opt "OCAMLOPTFLAGS" with
  | Some var ->
    String.split_on_char ~sep:' ' var
    |> omit_dtimings_dprofile
    |> String.concat ~sep:" "
    |> Unix.putenv "OCAMLOPTFLAGS"
  | None -> ()
;;

let invoke_compiler ~filename ~compiler_flags ~output_prefix ~languages ~setup_hooks =
  let compiler_flags =
    adjust_compiler_flags ~filename ~compiler_flags ~output_prefix ~languages
  in
  adjust_ocamlopt_env ();
  setup_hooks ();
  Clflags.zero_alloc_check := Zero_alloc_annotations.Check.No_check;
  let compiler_flags = Array.of_list (compiler_flags @ [ "-warn-error"; "-a" ]) in
  let buffer = Buffer.create 4096 in
  let formatter = Format.formatter_of_buffer buffer in
  Location.formatter_for_warnings := formatter;
  let status =
    Optmaindriver.main
      (module Unix : Compiler_owee.Unix_intf.S)
      compiler_flags
      formatter
      ~flambda2:Flambda2.lambda_to_cmm
  in
  if status = 0
  then Fiber.return (Result.return ())
  else Fiber.return (Result.compiler_exn (Buffer.contents buffer))
;;

module Mapping_result = struct
  type t =
    { ir : string
    ; mappings : Ltf.Mappings.t
    }

  let create ~text_ir ~mappings = { ir = text_ir; mappings }

  let augment ~text_ir ~mappings { ir = old_text_ir; mappings = old_mappings } =
    { ir = old_text_ir ^ "\n\n" ^ text_ir; mappings = old_mappings @ mappings }
  ;;
end

let configure_printers_using_compiler_hooks ~languages ~filename ~results =
  let configure_one_printer ~language printer ir =
    Ltf.activate_tracking ();
    let temp_file =
      Filename.temp_file "ocaml-ir-dump-ast" (Language.camel_case_of_t language)
    in
    let channel = Out_channel.open_text temp_file in
    let ppf = Format.formatter_of_out_channel channel in
    Format.pp_set_margin ppf 120;
    let tracking_formatter = Ltf.Tracking_formatter.create ~file:filename ~ppf in
    printer ppf ir;
    Format.pp_print_flush ppf ();
    close_out channel;
    let channel = In_channel.open_text temp_file in
    let text_ir = In_channel.input_all channel in
    Sys.remove temp_file;
    let mappings = Ltf.Tracking_formatter.mappings tracking_formatter in
    results
      := match Language.Map.find_opt language !results with
         | None ->
           Language.Map.add language (Mapping_result.create ~text_ir ~mappings) !results
         | Some text_ir_and_mappings ->
           Language.Map.add
             language
             (Mapping_result.augment ~text_ir ~mappings text_ir_and_mappings)
             !results
  in
  let configure_for language =
    Language.Map.find language Printers.hooks
    |> List.iter ~f:(fun (Printers.T (hook, hook_specific_printer)) ->
         Compiler_hooks.register
           hook
           (configure_one_printer ~language hook_specific_printer))
  in
  Language.Set.iter
    (function
     | Language.Assembly -> if not (Language.Set.mem Cmm languages) then configure_for Cmm
     | language -> configure_for language)
    languages
;;

let ir_and_map_for ~filename ~language results =
  let open Ir_with_mappings.Raw_or_file in
  match Language.Map.find_opt language results with
  | Some Mapping_result.{ ir; mappings = [] } ->
    Result.Fiber.return (Ir_with_mappings.create (Raw ir))
  | Some Mapping_result.{ ir; mappings } ->
    let ir_map = ir_map_of_mappings ~filename mappings in
    Result.Fiber.return (Ir_with_mappings.create ~mappings:(Raw ir_map) (Raw ir))
  | None -> Fiber.return (Result.no_ir_produced language)
;;

let ir_and_map_for_assembly ~filename ~output_prefix results =
  let open Ir_with_mappings.Raw_or_file in
  Result.Fiber.Let_syntax.Let_syntax.bind
    (ir_and_map_for ~filename ~language:Cmm results)
    ~f:(fun ir_and_map_cmm ->
    let ir_asm = File (Assembly_map.asm_file output_prefix) in
    Fiber.Let_syntax.Let_syntax.bind
      (Ir_with_mappings.mappings ir_and_map_cmm)
      ~f:(function
      | None -> Result.Fiber.return (Ir_with_mappings.create ir_asm)
      | Some ir_map_cmm ->
        let source_locations =
          List.map
            ~f:(fun entry -> Ir_map.Entry.label entry, Ir_map.Entry.source_location entry)
            ir_map_cmm
        in
        Fiber.Let_syntax.Let_syntax.bind
          (Assembly_map.get_mappings ~output_prefix source_locations)
          ~f:(fun (ir_asm_content, ir_map) ->
          Result.Fiber.return
            (Ir_with_mappings.create ~mappings:(Raw ir_map) (Raw ir_asm_content)))))
;;

module V2 = struct
  module Compile = struct
    type t = Ir_with_mappings.t Language.Map.t

    let t_of_sexp = Language.Map.t_of_sexp Ir_with_mappings.t_of_sexp
    let sexp_of_t = Language.Map.sexp_of_t Ir_with_mappings.sexp_of_t

    let run ~filename ~compiler_flags ~languages ~output_prefix =
      let results = ref Language.Map.empty in
      Let_syntax.bind
        (invoke_compiler
           ~filename
           ~languages
           ~compiler_flags
           ~output_prefix
           ~setup_hooks:(fun () ->
           configure_printers_using_compiler_hooks ~languages ~filename ~results))
        ~f:(fun () ->
          Language.Set.fold
            (fun language acc ->
              Result.Fiber.Let_syntax.Let_syntax.bind acc ~f:(fun acc ->
                Result.Fiber.Let_syntax.Let_syntax.map
                  (match language with
                   | Assembly -> ir_and_map_for_assembly ~filename ~output_prefix !results
                   | _ -> ir_and_map_for ~filename ~language !results)
                  ~f:(fun ir_and_map -> Language.Map.add language ir_and_map acc)))
            languages
            (Result.Fiber.return Language.Map.empty))
    ;;
  end
end

module V1 = struct
  module Compile = struct
    type t = unit [@@deriving sexp]

    include struct
      let _ = fun (_ : t) -> ()
      let t_of_sexp = (unit_of_sexp : Sexplib0.Sexp.t -> t)
      let _ = t_of_sexp
      let sexp_of_t = (sexp_of_unit : t -> Sexplib0.Sexp.t)
      let _ = sexp_of_t
    end [@@ocaml.doc "@inline"] [@@merlin.hide]

    let language_of_arg : string -> Language.t option = function
      | "-dcfg" -> Some Cfg
      | "-dcmm" -> Some Cmm
      | "-dlambda" -> Some Lambda
      | "-dlinear" -> Some Linear
      | "-drawflambda" -> Some Raw_flambda2
      | "-drawlambda" -> Some Raw_lambda
      | "-dflambda" -> Some Flambda2
      | "-dtypedtree" -> Some Typed_tree
      | "-dparsetree" -> Some Parse_tree
      | "-S" -> Some Assembly
      | _ -> None
    ;;

    let infer_languages_from_compiler_args compiler_flags =
      let languages = ref Language.Set.empty in
      let compiler_flags =
        compiler_flags
        |> List.filter_map ~f:(fun arg ->
             match language_of_arg arg with
             | None -> Some arg
             | Some language ->
               languages := Language.Set.add language !languages;
               None)
      in
      compiler_flags, !languages
    ;;

    let run ~filename ~compiler_flags ~output_prefix =
      let compiler_flags, languages = infer_languages_from_compiler_args compiler_flags in
      Result.Fiber.Let_syntax.Let_syntax.bind
        (V2.Compile.run ~filename ~output_prefix ~languages ~compiler_flags)
        ~f:(fun result ->
        let ir_output_path =
          if List.exists ~f:(String.equal "-impl") compiler_flags
          then output_prefix ^ ".cmx.dump"
          else output_prefix ^ ".cmi.dump"
        in
        match Language.Map.cardinal result with
        | 0 ->
          Fiber.return (Result.ocaml_ir_comp_error (Error.of_string "No ir returned"))
        | 1 ->
          let _, ir_and_map = Language.Map.choose result in
          Fiber.Let_syntax.Let_syntax.bind (Ir_with_mappings.ir ir_and_map) ~f:(fun ir ->
            Fiber.Let_syntax.Let_syntax.bind
              (Ir_with_mappings.mappings ir_and_map)
              ~f:(fun ir_map ->
              Fiber.Let_syntax.Let_syntax.bind
                (match ir_map with
                 | Some ir_map ->
                   let path = output_prefix ^ ".ir_map" in
                   let sexp = (Ir_map.sexp_of_raw [@merlin.hide]) ir_map in
                   let content = Sexplib.Sexp.to_string_hum sexp in
                   Fiber.Io.create_file_write_content ~path ~content
                 | None -> Fiber.return ())
                ~f:(fun () ->
                  Fiber.Let_syntax.Let_syntax.bind
                    (Fiber.Io.create_file_write_content ~path:ir_output_path ~content:ir)
                    ~f:(fun () -> Result.Fiber.return ()))))
        | _ ->
          Fiber.return
            (Result.ocaml_ir_comp_error
               (Error.of_string "Ocaml_ir_comp v1 show_ir can only show one ir")))
    ;;
  end
end
