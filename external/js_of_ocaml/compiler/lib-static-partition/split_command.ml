open! Js_of_ocaml_compiler.Stdlib
open Js_of_ocaml_compiler
open Cmdliner

let root =
  let parse s =
    let key, unit_name =
      match String.lsplit2 s ~on:'=' with
      | Some (key, unit_name) -> key, unit_name
      | None -> s, s
    in
    Ok { Split.key; unit_name }
  in
  let print formatter { Split.key; unit_name } =
    if String.equal key unit_name
    then Format.pp_print_string formatter unit_name
    else Format.fprintf formatter "%s=%s" key unit_name
  in
  Arg.conv (parse, print)

let command ~info ~backend ~effects_backends ~default_effects =
  let is_wasm =
    match (backend : Split.Backend.t) with
    | Javascript -> false
    | Wasm _ -> true
  in
  let files =
    let doc =
      if is_wasm
      then
        "The $(i,.cmo) and $(i,.cma) files of the program, in initialization order, and \
         its JavaScript runtime files ($(i,.js) files and builtins). Wasm runtime files \
         ($(i,.wat), $(i,.wasm)) are accepted and ignored: build the Wasm runtime with \
         $(b,wasm_of_ocaml build-runtime)."
      else
        "The $(i,.cmo) and $(i,.cma) files of the program, in initialization order, and \
         its runtime files ($(i,.js) files and builtins such as $(i,+dynlink.js))."
    in
    Arg.(non_empty & pos_all string [] & info [] ~docv:"FILE" ~doc)
  in
  let main_unit =
    let doc = "The compilation unit at the root of the eager code." in
    Arg.(required & opt (some string) None & info [ "main" ] ~docv:"UNIT" ~doc)
  in
  let roots =
    let doc =
      "A deferred root: compilation unit $(i,UNIT) and what only it needs, loaded under \
       the name $(i,KEY) (default: $(i,UNIT)). Repeat for several roots."
    in
    Arg.(non_empty & opt_all root [] & info [ "root" ] ~docv:"[KEY=]UNIT" ~doc)
  in
  let strategy =
    let doc =
      "How to split the program: $(b,modules) compiles the eager code and each root \
       separately; $(b,late) optimizes them as one program before splitting it (profile \
       3 only)."
    in
    Arg.(
      value
      & opt (enum [ "modules", Split.Modules; "late", Split.Late ]) Split.Modules
      & info [ "strategy" ] ~docv:"STRATEGY" ~doc)
  in
  let require_deferred_units =
    let doc =
      "Reject the split if $(i,UNIT) is reachable from eager code or from several roots \
       (using the values units require, not their interface imports)."
    in
    Arg.(value & opt_all string [] & info [ "require-deferred-unit" ] ~docv:"UNIT" ~doc)
  in
  let check_deferred_handles =
    let doc =
      "Reject runtime dependency paths from unit $(i,HANDLE) to the units of \
       $(i,ARCHIVE) (an input), without forbidding eager uses of those units."
    in
    Arg.(
      value
      & opt_all (pair ~sep:'=' string file) []
      & info [ "check-deferred-handle" ] ~docv:"HANDLE=ARCHIVE" ~doc)
  in
  let output_dir =
    let doc = "Write the outputs and $(i,report.json) to directory $(docv)." in
    Arg.(required & opt (some string) None & info [ "o" ] ~docv:"DIR" ~doc)
  in
  let profile =
    let doc = "Set optimization profile : [$(docv)] (default: 3)." in
    let profile =
      List.map Profile.all ~f:(fun p -> string_of_int (Profile.to_int p), p)
    in
    Arg.(value & opt (enum profile) Profile.O3 & info [ "opt" ] ~docv:"NUM" ~doc)
  in
  let effects =
    let doc = "Select an implementation of effect handlers; must match the runtime." in
    Arg.(
      value
      & opt (enum effects_backends) default_effects
      & info [ "effects" ] ~docv:"KIND" ~doc)
  in
  let fs_files =
    let doc = "Embed file $(docv) in the runtime (JavaScript only)." in
    Arg.(value & opt_all string [] & info [ "file" ] ~docv:"FILE" ~doc)
  in
  let static_env =
    let doc = "Set the static environment variable $(i,NAME) to $(i,VALUE)." in
    Arg.(
      value
      & opt_all (list (pair ~sep:'=' string string)) []
      & info [ "setenv" ] ~docv:"NAME=VALUE" ~doc)
  in
  let run
      common
      params
      files
      main_unit
      roots
      strategy
      require_deferred_units
      check_deferred_handles
      output_dir
      profile
      effects
      fs_files
      static_env =
    Jsoo_cmdline.Arg.eval common;
    List.iter (List.concat params) ~f:(fun (s, v) -> Config.Param.set s v);
    let inputs, runtime_files =
      List.partition files ~f:(fun file ->
          Filename.check_suffix file ".cmo" || Filename.check_suffix file ".cma")
    in
    List.iter runtime_files ~f:(fun file ->
        if
          not
            (Option.is_some (Builtins.find file)
            || Filename.check_suffix file ".js"
            || is_wasm
               && List.exists [ ".wat"; ".wast"; ".wasm" ] ~f:(Filename.check_suffix file)
            )
        then failwith (Printf.sprintf "Unexpected input file %s" file));
    Split.run
      { backend
      ; strategy
      ; profile
      ; effects
      ; inputs
      ; runtime_files
      ; fs_files
      ; static_env = List.concat static_env
      ; main_unit
      ; roots
      ; require_deferred_units
      ; check_deferred_handles
      ; output_dir
      }
  in
  Cmd.v
    info
    Term.(
      const run
      $ Lazy.force Jsoo_cmdline.Arg.t
      $ Jsoo_cmdline.Arg.set_param
      $ files
      $ main_unit
      $ roots
      $ strategy
      $ require_deferred_units
      $ check_deferred_handles
      $ output_dir
      $ profile
      $ effects
      $ fs_files
      $ static_env)
