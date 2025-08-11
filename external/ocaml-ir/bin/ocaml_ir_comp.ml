open Cmdliner
open Sexplib.Std
open Ocaml_ir_fiber
open Ocaml_ir_common.Std

module Stable = struct
  module V1 = struct
    let do_compile filename output_prefix compiler_flags =
      let compiler_flags =
        Parsexp.Conv_single.parse_string_exn compiler_flags (list_of_sexp string_of_sexp)
      in
      Fiber.block_on (fun () ->
        Let_syntax.map
          (Compile.V1.Compile.run ~filename ~compiler_flags ~output_prefix)
          ~f:(fun out ->
          print_string
            (Sexplib.Sexp.to_string_hum
               ((fun [@merlin.hide] x__002_ ->
                  Ocaml_ir_common.Result.sexp_of_t sexp_of_unit x__002_)
                  out))))
    ;;

    let filename =
      let open Arg in
      required
      & pos 0 (some string) None
      & info
          []
          ~docv:"FILE"
          ~doc:
            "Path to the file to be compiled (absolute or relative to the current \
             working directory). Supported are .{ml,mli} files, with .mli only \
             supporting a subset of the IR languages."
    ;;

    let output_prefix =
      let open Arg in
      required
      & opt (some string) None
      & info [ "o" ] ~docv:"OUTPUT_PREFIX" ~doc:"Output files prefix (can include path)."
    ;;

    let flags =
      let open Arg in
      required
      & opt (some string) None
      & info [ "f" ] ~docv:"FLAGS" ~doc:"Sexp of the list of compiler flags."
    ;;

    let subcommands =
      [ ( "get-ir"
        , "Save the ir to a file"
        , let open Term in
          const do_compile $ filename $ output_prefix $ flags )
      ]
    ;;

    let execute_cli argv =
      match subcommands with
      | (name, doc, term) :: _ ->
        let default, info = term, Cmd.info ~doc name in
        let choices =
          List.map subcommands ~f:(fun (name, doc, term) ->
            Cmd.v (Cmd.info ~doc name) term)
        in
        let result = Cmd.group ~default info choices in
        Cmd.eval ~argv:(Array.of_list argv) result
      | [] -> failwith "BUG: no subcommands provided"
    ;;
  end

  module V2 = struct
    let print_result ~output sexp =
      match output with
      | None -> Fiber.return (print_string (Sexplib.Sexp.to_string_hum sexp))
      | Some path ->
        Fiber.Io.create_file_write_content ~path ~content:(Sexplib.Sexp.to_string sexp)
    ;;

    let inlining_report filename output_prefix compiler_flags as_org_mode output =
      let compiler_flags =
        Parsexp.Conv_single.parse_string_exn compiler_flags (list_of_sexp string_of_sexp)
      in
      Fiber.block_on (fun () ->
        Let_syntax.bind
          (Inlining_report.V1.run ~filename ~output_prefix ~compiler_flags ~as_org_mode)
          ~f:(fun result ->
          print_result
            ~output
            ((fun [@merlin.hide] x__004_ ->
               Ocaml_ir_common.Result.sexp_of_t
                 Ocaml_ir_common.Inlining_report.sexp_of_t
                 x__004_)
               result)))
    ;;

    let find_allocations filename output_prefix compiler_flags emit_version output =
      let compiler_flags =
        Parsexp.Conv_single.parse_string_exn compiler_flags (list_of_sexp string_of_sexp)
      in
      Fiber.block_on (fun () ->
        Let_syntax.bind
          (Find_allocations.V1.run ~filename ~output_prefix ~compiler_flags)
          ~f:(fun result ->
          let result =
            Ocaml_ir_common.Result.map
              result
              ~f:(Ocaml_ir_common.Allocations.serialize emit_version)
          in
          print_result
            ~output
            ((fun [@merlin.hide] x__006_ ->
               Ocaml_ir_common.Result.sexp_of_t Sexplib.Sexp.sexp_of_t x__006_)
               result)))
    ;;

    let do_compile filename output_prefix compiler_flags languages output =
      let compiler_flags =
        Parsexp.Conv_single.parse_string_exn compiler_flags (list_of_sexp string_of_sexp)
      in
      let languages =
        List.map languages ~f:Ocaml_ir_common.Language.t_of_string_exn
        |> Ocaml_ir_common.Language.Set.of_list
      in
      Fiber.block_on (fun () ->
        Let_syntax.bind
          (Compile.V2.Compile.run ~filename ~compiler_flags ~languages ~output_prefix)
          ~f:(fun out ->
          print_result
            ~output
            ((fun [@merlin.hide] x__008_ ->
               Ocaml_ir_common.Result.sexp_of_t Compile.V2.Compile.sexp_of_t x__008_)
               out)))
    ;;

    let experimental_shape filename output_prefix compiler_flags output =
      let compiler_flags =
        Parsexp.Conv_single.parse_string_exn compiler_flags (list_of_sexp string_of_sexp)
      in
      Fiber.block_on (fun () ->
        Let_syntax.bind
          (Experimental.shape ~filename ~compiler_flags ~output_prefix)
          ~f:(fun out ->
          print_result
            ~output
            ((fun [@merlin.hide] x__010_ ->
               Ocaml_ir_common.Result.sexp_of_t sexp_of_string x__010_)
               out)))
    ;;

    let perf_preprocess buildid_cache_dir binary perf_data output =
      Fiber.block_on (fun () ->
        Fiber.Let_syntax.Let_syntax.bind
          (Perf_report.summarize_perf_data
             ?buildid_cache_dir
             ?binary
             ~force_recompute:true
             perf_data)
          ~f:(fun perf_data_summarized ->
          let perf_data_summarized =
            Ocaml_ir_common.Result.map ~f:(fun _ -> ()) perf_data_summarized
          in
          print_result
            ~output
            ((fun [@merlin.hide] x__012_ ->
               Ocaml_ir_common.Result.sexp_of_t sexp_of_unit x__012_)
               perf_data_summarized)))
    ;;

    let perf_list_events data output =
      Fiber.block_on (fun () ->
        let open Ocaml_ir_common.Perf_counters in
        Let_syntax.bind (Perf_report.summarize_perf_data data) ~f:(fun summary ->
          let events =
            Ocaml_ir_common.Result.map ~f:Perf_report.event_types_in_project summary
          in
          print_result
            ~output
            ((fun [@merlin.hide] x__014_ ->
               Ocaml_ir_common.Result.sexp_of_t Event_types_in_project.sexp_of_t x__014_)
               events)))
    ;;

    let perf_events_in_file filename _output_prefix _compiler_flags event data output =
      Fiber.block_on (fun () ->
        let open Ocaml_ir_common.Perf_counters in
        let event = Event_type.of_string event in
        Let_syntax.bind (Perf_report.summarize_perf_data data) ~f:(fun summary ->
          let events =
            Ocaml_ir_common.Result.map
              ~f:(Perf_report.events_in_file ~filename ~event)
              summary
          in
          print_result
            ~output
            ((fun [@merlin.hide] x__016_ ->
               Ocaml_ir_common.Result.sexp_of_t Events_for_a_file.sexp_of_t x__016_)
               events)))
    ;;

    let perf_event_count_per_files event data output =
      Fiber.block_on (fun () ->
        let open Ocaml_ir_common.Perf_counters in
        let event = Event_type.of_string event in
        Let_syntax.bind (Perf_report.summarize_perf_data data) ~f:(fun summary ->
          let events =
            Ocaml_ir_common.Result.map
              ~f:(Perf_report.event_count_per_files ~event)
              summary
          in
          print_result
            ~output
            ((fun [@merlin.hide] x__018_ ->
               Ocaml_ir_common.Result.sexp_of_t Event_count_per_files.sexp_of_t x__018_)
               events)))
    ;;

    let filename =
      let open Arg in
      required
      & pos 0 (some string) None
      & info
          []
          ~docv:"FILE"
          ~doc:
            "Path to the file to be compiled (absolute or relative to the current \
             working directory). Supported are .{ml,mli} files, with .mli only \
             supporting a subset of the IR languages."
    ;;

    let output_prefix =
      let open Arg in
      required
      & opt (some string) None
      & info [ "o" ] ~docv:"OUTPUT_PREFIX" ~doc:"Output files prefix (can include path)."
    ;;

    let flags =
      let open Arg in
      required
      & opt (some string) None
      & info [ "f" ] ~docv:"FLAGS" ~doc:"Sexp of the list of compiler flags."
    ;;

    let languages =
      let open Arg in
      non_empty
      & opt (list string) []
      & info
          [ "l"; "languages" ]
          ~docv:"LANGUAGES"
          ~doc:"Comma separated list of IR languages to retreive."
    ;;

    let output =
      let open Arg in
      value
      & opt (some string) None
      & info
          [ "output" ]
          ~docv:"OUTPUT"
          ~doc:"If specified the output while be save in OUTPUT."
    ;;

    let as_org_mode =
      let open Arg in
      value & flag & info [ "org-mode" ] ~doc:"Output as an org mode file."
    ;;

    let perf_data =
      let open Arg in
      required
      & opt (some string) None
      & info [ "data" ] ~docv:"FILE" ~doc:"the perf data file containing the perf profile"
    ;;

    let _output_filename =
      let open Arg in
      required
      & opt (some string) None
      & info
          [ "output-filename" ]
          ~docv:"FILE"
          ~doc:"the destination of the preprocessed perf data"
    ;;

    let event =
      let open Arg in
      required
      & opt (some string) None
      & info
          [ "event" ]
          ~docv:"STRING"
          ~doc:"the perf event you want to count (use perf list to see events)"
    ;;

    let buildid_cache_dir =
      let open Arg in
      value
      & opt (some string) None
      & info
          [ "buildid-cache" ]
          ~docv:"DIR"
          ~doc:"Path to the buildid cache folder (usually in ~/.debug)"
    ;;

    let binary =
      let open Arg in
      value
      & opt (some string) None
      & info
          [ "binary" ]
          ~docv:"BINARY"
          ~doc:"Path to the binary that was sampled by perf"
    ;;

    let version =
      let open Arg in
      value
      & opt (enum [ "1", `V1; "2", `V2 ]) `V1
      & info
          [ "emit-version" ]
          ~docv:"number"
          ~doc:"Maximal version of the output ocaml-ir-comp can preduce (default 1)"
    ;;

    let as_compiler c =
      let open Term in
      c $ filename $ output_prefix $ flags
    ;;

    let subcommands =
      [ ( "get-ir"
        , "Save the ir to a file"
        , let open Term in
          as_compiler (const do_compile) $ languages $ output )
      ; ( "find-allocations"
        , "Find hypothetical allocations"
        , let open Term in
          as_compiler (const find_allocations) $ version $ output )
      ; ( "find-inlining-report"
        , "Retrieve inlining report"
        , let open Term in
          as_compiler (const inlining_report) $ as_org_mode $ output )
      ; ( "perf-events-in-file"
        , "Retrieve perf data"
        , let open Term in
          as_compiler (const perf_events_in_file) $ event $ perf_data $ output )
      ; ( "perf-list-events"
        , "List perf events recorded for a file"
        , let open Term in
          const perf_list_events $ perf_data $ output )
      ; ( "perf-preprocess"
        , "Prepare perf data"
        , let open Term in
          const perf_preprocess $ buildid_cache_dir $ binary $ perf_data $ output )
      ; ( "perf-event-count-per-files"
        , "Return the count of the specified event per each file where it was seen"
        , let open Term in
          const perf_event_count_per_files $ event $ perf_data $ output )
      ; ( "experimental-shape"
        , "EXPERIMENTAL: return the shape information contained in a file"
        , let open Term in
          as_compiler (const experimental_shape) $ output )
      ]
    ;;

    let execute_cli argv =
      match subcommands with
      | (name, doc, term) :: _ ->
        let default, info = term, Cmd.info ~doc name in
        let choices =
          List.map subcommands ~f:(fun (name, doc, term) ->
            Cmd.v (Cmd.info ~doc name) term)
        in
        let result = Cmd.group ~default info choices in
        Cmd.eval ~argv:(Array.of_list argv) result
      | [] -> failwith "BUG: no subcommands provided"
    ;;
  end
end

let () =
  let man_page =
    ( ("ocaml-ir-comp", 1, "", "", "ocaml-ir-comp Manual")
    , [ `S Manpage.s_name
      ; `P "ocaml-ir-comp - interact with the compiler to generate irs"
      ; `S Manpage.s_synopsis
      ; `P "ocaml-ir-comp VERSION SUBCOMMAND ..."
      ; `S "VERSION"
      ; `I ("v1", "use ocaml ir comp v1.")
      ; `S "v1 SUBCOMMANDS"
      ; `Blocks (List.map Stable.V1.subcommands ~f:(fun (name, doc, _) -> `I (name, doc)))
      ; `I ("v2", "use ocaml ir comp v1.")
      ; `S "v2 SUBCOMMANDS"
      ; `Blocks (List.map Stable.V2.subcommands ~f:(fun (name, doc, _) -> `I (name, doc)))
      ] )
  in
  let result =
    match Array.to_list Sys.argv with
    | name :: "v1" :: argv -> Stable.V1.execute_cli (name :: argv)
    | name :: "v2" :: argv -> Stable.V2.execute_cli (name :: argv)
    | _ ->
      Manpage.print `Auto Format.err_formatter man_page;
      0
  in
  exit result
;;
