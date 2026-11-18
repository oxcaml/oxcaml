open! Js_of_ocaml_compiler.Stdlib
module Jsoo = Js_of_ocaml_compiler
module C = Jsoo.Code
module Split = Jsoo_static_partition.Split

let check program =
  match Split.For_testing.split_late_exn program with
  | Partitioned _ -> print_endline "Partitioned"
  | Terminal_eager _ -> print_endline "Terminal eager"
  | exception Failure message -> print_endline ("Error: " ^ message)

let%expect_test "normal exits and retained markers cannot use terminal eager" =
  check C.empty;
  [%expect {| Error: Expected deferred split anchors |}];
  (* A reachable raise must not hide another path's ordinary completion. *)
  let choice = C.Var.fresh () in
  let exception_ = C.Var.fresh () in
  let call variable name =
    C.Let (variable, Prim (Extern (name, None), [ Pc (Int Jsoo.Targetint.zero) ]))
  in
  let entry : C.block =
    { params = []
    ; body = [ call choice "terminal_choice"; call exception_ "terminal_exception" ]
    ; branch = Cond (choice, (1, []), (2, []))
    }
  in
  let stopped : C.block = { params = []; body = []; branch = Stop } in
  let raising = { stopped with branch = Raise (exception_, `Normal) } in
  check
    { C.start = 0
    ; free_pc = 3
    ; blocks = C.Addr.Map.(empty |> add 0 entry |> add 1 raising |> add 2 stopped)
    };
  [%expect {| Error: Expected deferred split anchors |}];
  (* Unreachable or malformed markers must still go through the strict splitter. *)
  let marker =
    { stopped with
      body = [ call (C.Var.fresh ()) (Jsoo.Parse_bytecode.split_anchor_for_root "lazy") ]
    }
  in
  let terminal = { raising with body = [ call exception_ "terminal_exception" ] } in
  check
    { C.start = 0
    ; free_pc = 2
    ; blocks = C.Addr.Map.(empty |> add 0 terminal |> add 1 marker)
    };
  [%expect {| Error: Expected deferred split anchors |}];
  (* Without markers, an initializer that always raises is terminal. *)
  check { C.start = 0; free_pc = 1; blocks = C.Addr.Map.(empty |> add 0 terminal) };
  [%expect {| Terminal eager |}]

let run
    ?(backend = Split.Backend.Javascript)
    ?(strategy = Split.Modules)
    ?(profile = Jsoo.Profile.O3)
    ?(effects = `Disabled)
    ?(fs_files = [])
    roots =
  match
    Split.run
      { backend
      ; strategy
      ; profile
      ; effects
      ; inputs = []
      ; runtime_files = []
      ; fs_files
      ; static_env = []
      ; main_unit = "Main"
      ; roots = List.map roots ~f:(fun (key, unit_name) -> { Split.key; unit_name })
      ; require_deferred_units = []
      ; check_deferred_handles = []
      ; output_dir = "/nonexistent/output"
      }
  with
  | () -> print_endline "Ok"
  | exception Failure message -> print_endline ("Error: " ^ message)

let%expect_test "reject invalid options before reading inputs" =
  run [];
  [%expect {| Error: At least one deferred root is required |}];
  run [ "a", "Not-a-unit" ];
  [%expect {| Error: Invalid root unit "Not-a-unit" |}];
  run [ "a/b", "A" ];
  [%expect {| Error: Invalid root key "a/b" |}];
  run [ "a", "A"; "a", "B" ];
  [%expect {| Error: Duplicate root key a |}];
  run [ "a", "A"; "b", "A" ];
  [%expect {| Error: Duplicate root unit A |}];
  run ~strategy:Late ~profile:Jsoo.Profile.O2 [ "a", "A" ];
  [%expect
    {| Error: The late strategy requires profile 3 and either Wasm with JSPI or JavaScript without effects |}];
  run ~strategy:Late ~effects:`Cps [ "a", "A" ];
  [%expect
    {| Error: The late strategy requires profile 3 and either Wasm with JSPI or JavaScript without effects |}];
  run ~effects:`Jspi [ "a", "A" ];
  [%expect
    {| Error: The selected effects backend is incompatible with the output backend |}];
  let wasm =
    Split.Backend.Wasm
      { init = (fun () -> ())
      ; compile = (fun _ ~unit_name:_ ~profile:_ -> failwith "unreachable")
      ; startup = (fun ~profile:_ -> failwith "unreachable")
      }
  in
  run ~backend:wasm ~effects:`Jspi ~fs_files:[ "a.txt" ] [ "a", "A" ];
  [%expect {| Error: Embedded files are only supported for JavaScript |}];
  run ~backend:wasm ~strategy:Late ~effects:`Disabled [ "a", "A" ];
  [%expect
    {| Error: The late strategy requires profile 3 and either Wasm with JSPI or JavaScript without effects |}]
