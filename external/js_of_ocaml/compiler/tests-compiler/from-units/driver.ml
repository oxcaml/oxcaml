open Js_of_ocaml_compiler
open Js_of_ocaml_compiler.Stdlib

type action =
  | Check_invariant
  | Optimize_for_partition
  | Optimize_for_wasm
  | Print_registry_calls
  | Print_cmis
  | Print_debug_info
  | Print_closures
  | Print_ir
  | Print_split_anchors
  | Print_trampolines
  | Emit_js of string

type state =
  { parsed : Parse_bytecode.one
  ; program : Code.program
  ; wasm_flow : Global_flow.info option
  }

let with_invariant enabled f =
  let previous = Debug.find "invariant" () in
  let set enabled =
    if enabled then Debug.enable "invariant" else Debug.disable "invariant"
  in
  set enabled;
  Fun.protect ~finally:(fun () -> set previous) f
;;

let parse ?exported_unit ~include_cmis ~debug ~deferred ~dependencies eager =
  let channels = ref [] in
  let open_file file =
    let ic = open_in_bin file in
    channels := ic :: !channels;
    ic
  in
  Fun.protect
    ~finally:(fun () -> List.iter !channels ~f:close_in_noerr)
    (fun () ->
      match eager, deferred, dependencies, exported_unit with
      | [ file ], [], [], None when Filename.check_suffix file ".cma" ->
        let ic = open_file file in
        (match Parse_bytecode.from_channel ic with
         | `Cma lib -> Parse_bytecode.from_cma ~include_cmis ~debug lib ic
         | `Cmo _ | `Exe -> failwith (file ^ ": expected an archive"))
      | _ ->
        (* An archive stands for its units, in order. *)
        let read_units files =
          List.concat_map files ~f:(fun file ->
            let ic = open_file file in
            match Parse_bytecode.from_channel ic with
            | `Cmo unit -> [ unit, ic ]
            | `Cma lib -> List.map lib.Cmo_format.lib_units ~f:(fun unit -> unit, ic)
            | `Exe -> failwith (file ^ ": expected a compilation unit or an archive"))
        in
        let deferred =
          List.map deferred ~f:(fun (id, files) ->
            Parse_bytecode.Deferred_code.create ~id (read_units files))
        in
        Parse_bytecode.from_units
          ?exported_unit
          ~include_cmis
          ~debug
          ~deferred
          ~dependencies:(read_units dependencies)
          (read_units eager |> Parse_bytecode.Code_unit.create))
;;

let iter_instrs program f =
  Code.Addr.Map.iter (fun _ block -> List.iter block.Code.body ~f) program.Code.blocks
;;

let print_sorted lines = List.iter (List.sort ~cmp:String.compare lines) ~f:print_endline

let print_registry_calls program =
  let calls = ref [] in
  iter_instrs program (function
    | Code.Let (_, Prim (Extern (prim, _), args))
      when List.mem
             ~eq:String.equal
             prim
             [ "caml_register_global"; "caml_get_global"; "caml_get_global_predef" ] ->
      let name =
        List.find_map args ~f:(function
          | Code.Pc (NativeString (Byte s | Utf (Utf8 s))) -> Some s
          | _ -> None)
      in
      calls := (prim ^ " " ^ Option.value name ~default:"?") :: !calls
    | _ -> ());
  print_sorted !calls
;;

let print_closures program =
  let closures = ref [] in
  iter_instrs program (function
    | Code.Let (_, Closure (_, _, (hint, loc))) ->
      let inline =
        match hint with
        | None -> "no hint"
        | Some hint ->
          (match hint.Optimization_hint.inline with
           | Always_inline -> "always"
           | Never_inline -> "never"
           | Hint_inline -> "hint"
           | Unroll _ -> "unroll"
           | Default_inline -> "default")
      in
      let location =
        match loc with
        | None -> "no location"
        | Some loc ->
          Printf.sprintf
            "%s:%d"
            (Filename.basename (Option.value loc.Parse_info.src ~default:"?"))
            loc.line
      in
      closures := (location ^ " " ^ inline) :: !closures
    | _ -> ());
  print_sorted !closures
;;

let print_split_anchors program flow =
  let anchors = ref [] in
  iter_instrs program (function
    | Code.Let (_, Prim (Extern (name, _), [ Pv thunk ])) ->
      (match Parse_bytecode.split_anchor_root name with
       | None -> ()
       | Some id ->
         let line =
           match Global_flow.get_unique_closure flow thunk with
           | None -> Printf.sprintf "Root [%s] has no unique initializer closure" id
           | Some (_, params) ->
             Printf.sprintf "Root [%s] initializer arity: [%d]" id (List.length params)
         in
         anchors := line :: !anchors)
    | _ -> ());
  print_sorted !anchors
;;

let has_trampoline program =
  let found = ref false in
  iter_instrs program (function
    | Code.Let (_, Prim (Extern (name, _), _))
      when String.equal name "caml_trampoline"
           || String.equal name "caml_trampoline_return" -> found := true
    | _ -> ());
  !found
;;

let javascript program =
  let buffer = Buffer.create 128 in
  Driver.f'
    ~standalone:false
    ~profile:Profile.O3
    ~link:`No
    (Pretty_print.to_buffer buffer)
    program;
  Buffer.contents buffer
;;

let write_file file contents =
  let oc = open_out_bin file in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> output_string oc contents)
;;

let run state = function
  | Check_invariant ->
    with_invariant true (fun () -> Code.invariant state.program);
    print_endline "IR invariants: [valid]";
    state
  | Optimize_for_partition ->
    { state with
      program = Driver.optimize_for_partition ~profile:Profile.O3 state.program
    ; wasm_flow = None
    }
  | Optimize_for_wasm ->
    let optimized, (_, info) =
      Driver.optimize_for_wasm ~profile:Profile.O3 ~shapes:false state.program
    in
    { state with program = optimized.program; wasm_flow = Some info }
  | Print_registry_calls ->
    print_registry_calls state.program;
    state
  | Print_cmis ->
    StringSet.iter print_endline state.parsed.cmis;
    state
  | Print_debug_info ->
    print_endline
      (if Parse_bytecode.Debug.is_empty state.parsed.debug
       then "Debug info: [empty]"
       else "Debug info: [present]");
    state
  | Print_closures ->
    print_closures state.program;
    state
  | Print_ir ->
    print_string
      (Format.asprintf
         "%a"
         (fun fmt p -> Code.Print.program fmt (fun _ _ -> "") p)
         state.program);
    state
  | Print_split_anchors ->
    let flow =
      match state.wasm_flow with
      | Some flow -> flow
      | None -> snd (Global_flow.f ~fast:false state.program)
    in
    print_split_anchors state.program flow;
    state
  | Print_trampolines ->
    Printf.printf "Has trampolines: [%b]\n" (has_trampoline state.program);
    state
  | Emit_js file ->
    write_file file (javascript state.program);
    state
;;

let usage =
  "driver.exe parse [OPTION...] [UNIT.cmo...] [-root ID UNIT.cmo...]... [-dependency \
   FILE]... [ACTION...]\n\n\
   Parses eager units, or a single archive, and the deferred roots jointly. Actions run \
   in command-line order on the resulting program."
;;

let main argv =
  let eager = ref [] in
  let deferred = ref [] in
  let dependencies = ref [] in
  let exported_unit = ref None in
  let include_cmis = ref false in
  let debug = ref false in
  let actions = ref [] in
  let add_file file =
    match !deferred with
    | [] -> eager := file :: !eager
    | (id, files) :: roots -> deferred := (id, file :: files) :: roots
  in
  let action name action doc =
    name, Arg.Unit (fun () -> actions := action :: !actions), doc
  in
  let specs =
    Arg.align
      [ ( "-root"
        , Arg.String (fun id -> deferred := (id, []) :: !deferred)
        , "ID Start deferred root ID; subsequent units belong to it" )
      ; ( "-dependency"
        , Arg.String (fun file -> dependencies := file :: !dependencies)
        , "FILE Let the compiler place FILE, a unit or an archive, where it is needed \
           (repeatable)" )
      ; ( "-exported-unit"
        , Arg.String
            (fun name ->
              exported_unit := Some (name :: Option.value !exported_unit ~default:[]))
        , "UNIT Register only UNIT (repeatable); default: register every unit" )
      ; "-no-exports", Arg.Unit (fun () -> exported_unit := Some []), " Register no unit"
      ; "-include-cmis", Arg.Set include_cmis, " Record the CMIs of the parsed units"
      ; "-debug", Arg.Set debug, " Read debug information"
      ; ( "-target"
        , Arg.Symbol
            ( [ "js"; "wasm" ]
            , function
              | "wasm" -> Config.set_target `Wasm
              | _ -> Config.set_target `JavaScript )
        , " Compilation target (default: js)" )
      ; ( "-invariants"
        , Arg.Unit (fun () -> Debug.enable "invariant")
        , " Check IR invariants during parsing and optimization" )
      ; action "-check-invariant" Check_invariant " Check IR invariants"
      ; action
          "-optimize-for-partition"
          Optimize_for_partition
          " Run Driver.optimize_for_partition at O3"
      ; action
          "-optimize-for-wasm"
          Optimize_for_wasm
          " Run Driver.optimize_for_wasm at O3"
      ; action
          "-print-registry-calls"
          Print_registry_calls
          " Print global registrations and lookups"
      ; action "-print-cmis" Print_cmis " Print the CMIs to embed"
      ; action "-print-debug-info" Print_debug_info " Print whether debug info is present"
      ; action
          "-print-closures"
          Print_closures
          " Print each closure's location and inlining hint"
      ; action "-print-ir" Print_ir " Print the IR"
      ; action
          "-print-split-anchors"
          Print_split_anchors
          " Print each deferred root marker and its closure's arity"
      ; action
          "-print-trampolines"
          Print_trampolines
          " Print whether the IR calls trampolines"
      ; ( "-emit-js"
        , Arg.String (fun file -> actions := Emit_js file :: !actions)
        , "FILE Write JavaScript for the IR to FILE" )
      ]
  in
  Config.set_target `JavaScript;
  Config.set_effects_backend `Disabled;
  Config.Flag.set "shortvar" false;
  Debug.disable "invariant";
  match Array.to_list argv with
  | _ :: "parse" :: _ ->
    (try Arg.parse_argv ~current:(ref 1) argv specs add_file usage with
     | Arg.Help message ->
       print_string message;
       exit 0
     | Arg.Bad message ->
       prerr_string message;
       exit 2);
    let parsed =
      try
        parse
          ?exported_unit:!exported_unit
          ~include_cmis:!include_cmis
          ~debug:!debug
          ~deferred:(List.rev_map !deferred ~f:(fun (id, files) -> id, List.rev files))
          ~dependencies:(List.rev !dependencies)
          (List.rev !eager)
      with
      | Invalid_argument message ->
        prerr_endline message;
        exit 1
    in
    let state = { parsed; program = parsed.code; wasm_flow = None } in
    let (_ : state) = List.fold_left (List.rev !actions) ~init:state ~f:run in
    ()
  | _ ->
    prerr_string (Arg.usage_string specs usage);
    exit 2
;;

let () = main Sys.argv
