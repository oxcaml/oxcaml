open! Js_of_ocaml_compiler.Stdlib
module Jsoo = Js_of_ocaml_compiler
module C = Jsoo.Code
module Late = Jsoo_static_partition.Late_partition

let initialize () =
  C.Var.reset ();
  Jsoo.Config.set_target `Wasm;
  Jsoo.Config.set_effects_backend `Jspi;
  Jsoo.Linker.reset ();
  Jsoo.Primitive.register
    (Jsoo.Parse_bytecode.split_anchor_for_root "lazy")
    `Mutator
    None
    (Some 1);
  Jsoo.Primitive.register "late_test_sink" `Mutator None (Some 1)

let anchor ?(id = "lazy") thunk =
  C.Let
    ( C.Var.fresh ()
    , Prim (Extern (Jsoo.Parse_bytecode.split_anchor_for_root id, None), [ Pv thunk ]) )

let sink value =
  C.Let (C.Var.fresh (), Prim (Extern ("late_test_sink", None), [ Pv value ]))

let block ?(params = []) body branch : C.block = { params; body; branch }

let blocks l =
  List.fold_left l ~init:C.Addr.Map.empty ~f:(fun blocks (pc, block) ->
      C.Addr.Map.add pc block blocks)

let program ~read_before_anchor ~anchors =
  initialize ();
  let zero = C.Var.fresh () in
  let cell = C.Var.fresh () in
  let snapshot = C.Var.fresh () in
  let argument = C.Var.fresh () in
  let thunk = C.Var.fresh () in
  let read = C.Let (snapshot, Field (cell, 0, Non_float)) in
  let root =
    block
      ([ C.Let (zero, Constant (Int Jsoo.Targetint.zero))
       ; C.Let (cell, Block (0, [| zero |], NotArray, Maybe_mutable))
       ]
      @ (if read_before_anchor then [ read ] else [])
      @ [ C.Let (thunk, Closure ([ argument ], (1, []), (None, None))) ]
      @ List.init ~len:anchors ~f:(fun _ -> anchor thunk))
      Stop
  in
  let delayed =
    block
      ((if read_before_anchor then [] else [ read ]) @ [ sink snapshot ])
      (Return argument)
  in
  { C.start = 0; free_pc = 2; blocks = blocks [ 0, root; 1, delayed ] }

let field_reads (program : C.program) =
  C.Addr.Map.fold
    (fun _ (block : C.block) count ->
      count
      + List.length
          (List.filter block.body ~f:(function
            | C.Let (_, Field _) -> true
            | _ -> false)))
    program.blocks
    0

let definitions values =
  String.concat
    ~sep:", "
    (List.map values ~f:(fun (value : Late.Report.value) -> value.definition))

let names values =
  String.concat
    ~sep:", "
    (List.filter_map values ~f:(fun (value : Late.Report.value) -> value.name))

let print_capture ~read_before_anchor =
  let split = Late.split_exn (program ~read_before_anchor ~anchors:1) in
  Printf.printf
    "captures: [%s]; eager field reads: %d; lazy field reads: %d\n"
    (definitions (Late.report split).captures)
    (field_reads (Late.eager split))
    (field_reads (Late.lazy_ split))

let%expect_test "capture an eager snapshot, not a deferred read" =
  print_capture ~read_before_anchor:true;
  [%expect {| captures: [field read]; eager field reads: 1; lazy field reads: 1 |}];
  print_capture ~read_before_anchor:false;
  [%expect {| captures: [mutable block]; eager field reads: 0; lazy field reads: 2 |}]

let%expect_test "reject absent or duplicated anchors" =
  List.iter [ 0; 2 ] ~f:(fun anchors ->
      match Late.split_exn (program ~read_before_anchor:false ~anchors) with
      | (_ : Late.t) -> print_endline "Ok"
      | exception Failure message -> print_endline ("Error: " ^ message));
  [%expect
    {|
    Error: Expected exactly one deferred split anchor, found 0
    Error: Expected exactly one deferred split anchor, found 2
    |}]

let%expect_test "move a mutually recursive closure group together" =
  initialize ();
  let f = C.Var.fresh () in
  let g = C.Var.fresh () in
  let thunk = C.Var.fresh () in
  let f_arg = C.Var.fresh () in
  let g_arg = C.Var.fresh () in
  let thunk_arg = C.Var.fresh () in
  let f_result = C.Var.fresh () in
  let g_result = C.Var.fresh () in
  let thunk_result = C.Var.fresh () in
  let apply result fn argument =
    C.Let
      (result, Apply { f = fn; args = [ argument ]; exact = false; yielding = May_yield })
  in
  let program_blocks =
    blocks
      [ ( 0
        , block
            [ C.Let (f, Closure ([ f_arg ], (1, []), (None, None)))
            ; C.Let (g, Closure ([ g_arg ], (4, []), (None, None)))
            ; C.Let (thunk, Closure ([ thunk_arg ], (5, []), (None, None)))
            ; anchor thunk
            ]
            Stop )
      ; 1, block [] (Cond (f_arg, (2, []), (3, [])))
      ; 2, block [ apply f_result g f_arg ] (Return f_result)
      ; 3, block [] (Return f_arg)
      ; 4, block [ apply g_result f g_arg ] (Return g_result)
      ; 5, block [ apply thunk_result f thunk_arg ] (Return thunk_result)
      ]
  in
  let split = Late.split_exn { C.start = 0; free_pc = 6; blocks = program_blocks } in
  let report = Late.report split in
  Printf.printf
    "captures: %d; relocated closures: %d\n"
    (List.length report.captures)
    (List.length
       (List.filter report.relocated ~f:(fun (value : Late.Report.value) ->
            String.equal value.definition "closure")));
  [%expect {| captures: 0; relocated closures: 3 |}];
  let other = C.Var.fresh () in
  let other_arg = C.Var.fresh () in
  let other_result = C.Var.fresh () in
  let marker = Jsoo.Parse_bytecode.split_anchor_for_root "other" in
  Jsoo.Primitive.register marker `Mutator None (Some 1);
  let root = C.Addr.Map.find 0 program_blocks in
  let root =
    { root with
      body =
        root.body
        @ [ C.Let (other, Closure ([ other_arg ], (6, []), (None, None)))
          ; anchor ~id:"other" other
          ]
    }
  in
  let program_blocks =
    program_blocks
    |> C.Addr.Map.add 0 root
    |> C.Addr.Map.add 6 (block [ apply other_result g other_arg ] (Return other_result))
  in
  let shared =
    Late.split_many_exn { C.start = 0; free_pc = 7; blocks = program_blocks }
  in
  List.iter (Late.reports shared) ~f:(fun (id, (report : Late.Report.t)) ->
      Printf.printf
        "%s: captures: %d; relocated: %d\n"
        id
        (List.length report.captures)
        (List.length report.relocated));
  [%expect
    {|
    lazy: captures: 1; relocated: 1
    other: captures: 1; relocated: 1
    |}]

let%expect_test "a captured field promotes another root's movable closure dependency" =
  initialize ();
  let zero = C.Var.fresh () in
  let closure = C.Var.fresh_n "retained_closure" in
  let boxed = C.Var.fresh () in
  let field = C.Var.fresh_n "snapshot" in
  let argument = C.Var.fresh () in
  let definitions =
    [ C.Let (zero, Constant (Int Jsoo.Targetint.zero))
    ; C.Let (closure, Closure ([ argument ], (3, []), (None, None)))
    ; C.Let (boxed, Block (0, [| closure |], NotArray, Immutable))
    ; C.Let (field, Field (boxed, 0, Non_float))
    ]
  in
  let delayed, anchors =
    List.split
      (List.mapi
         [ "A", field; "B", closure ]
         ~f:(fun index (id, value) ->
           let thunk = C.Var.fresh () in
           let argument = C.Var.fresh () in
           Jsoo.Primitive.register
             (Jsoo.Parse_bytecode.split_anchor_for_root id)
             `Mutator
             None
             (Some 1);
           ( (index + 1, block [ sink value ] (Return argument))
           , [ C.Let (thunk, Closure ([ argument ], (index + 1, []), (None, None)))
             ; anchor ~id thunk
             ] )))
  in
  let program_blocks =
    blocks
      ((0, block (definitions @ List.concat anchors) Stop)
      :: (3, block [] (Return zero))
      :: delayed)
  in
  let split = Late.split_many_exn { C.start = 0; free_pc = 4; blocks = program_blocks } in
  List.iter (Late.reports split) ~f:(fun (id, (report : Late.Report.t)) ->
      Printf.printf
        "%s: captures: [%s]; rounds: %d; moved: %d\n"
        id
        (names report.captures)
        report.rounds
        (List.length report.relocated));
  [%expect
    {|
    A: captures: [snapshot]; rounds: 3; moved: 1
    B: captures: [retained_closure]; rounds: 3; moved: 1
    |}]

let%expect_test "joint ownership keeps shared closures and immutable identity eager" =
  initialize ();
  let shared_literal = C.Var.fresh_n "shared_literal" in
  let exclusive = C.Var.fresh_n "exclusive" in
  let shared_closure = C.Var.fresh_n "shared_closure" in
  let shared_block = C.Var.fresh_n "shared_block" in
  let shared_argument = C.Var.fresh () in
  let eager =
    [ C.Let (shared_literal, Constant (String "SHARED_LITERAL"))
    ; C.Let (exclusive, Constant (String "EXCLUSIVE_LITERAL"))
    ; C.Let (shared_closure, Closure ([ shared_argument ], (4, []), (None, None)))
    ; C.Let
        ( shared_block
        , Block (0, [| shared_literal; shared_closure |], NotArray, Immutable) )
    ]
  in
  let root_blocks, anchors =
    List.split
      (List.init ~len:3 ~f:(fun index ->
           let id = string_of_int index in
           let thunk = C.Var.fresh_n ("thunk_" ^ id) in
           let argument = C.Var.fresh () in
           Jsoo.Primitive.register
             (Jsoo.Parse_bytecode.split_anchor_for_root id)
             `Mutator
             None
             (Some 1);
           ( ( index + 1
             , block
                 (sink shared_block :: (if index = 0 then [ sink exclusive ] else []))
                 (Return argument) )
           , [ C.Let (thunk, Closure ([ argument ], (index + 1, []), (None, None)))
             ; anchor ~id thunk
             ] )))
  in
  let program_blocks =
    blocks
      ((0, block (eager @ List.concat anchors) Stop)
      :: (4, block [] (Return shared_literal))
      :: root_blocks)
  in
  let split = Late.split_many_exn { C.start = 0; free_pc = 5; blocks = program_blocks } in
  List.iter (Late.reports split) ~f:(fun (id, (report : Late.Report.t)) ->
      Printf.printf
        "%s: captures: [%s]; moved: [%s]\n"
        id
        (names report.captures)
        (names report.relocated));
  [%expect
    {|
    0: captures: [shared_block]; moved: [exclusive, thunk_0]
    1: captures: [shared_block]; moved: [thunk_1]
    2: captures: [shared_block]; moved: [thunk_2]
    |}]

let%expect_test "capture a value merged from an eager exception handler" =
  initialize ();
  let zero = C.Var.fresh () in
  let exception_ = C.Var.fresh () in
  let value = C.Var.fresh () in
  let thunk = C.Var.fresh () in
  let argument = C.Var.fresh () in
  let program_blocks =
    blocks
      [ ( 0
        , block
            [ C.Let (zero, Constant (Int Jsoo.Targetint.zero)) ]
            (Pushtrap ((1, []), exception_, (2, []))) )
      ; 1, block [] (Poptrap (3, [ zero ]))
      ; 2, block [] (Branch (3, [ exception_ ]))
      ; ( 3
        , block
            ~params:[ value ]
            [ C.Let (thunk, Closure ([ argument ], (4, []), (None, None))); anchor thunk ]
            Stop )
      ; 4, block [ sink value ] (Return argument)
      ]
  in
  let split = Late.split_exn { C.start = 0; free_pc = 5; blocks = program_blocks } in
  print_endline (definitions (Late.report split).captures);
  [%expect {| control-flow value |}]

let exception_capture_program kind =
  initialize ();
  Jsoo.Primitive.register "late_test_probe" `Mutator None (Some 1);
  Jsoo.Primitive.register "late_test_exception" `Mutator None (Some 1);
  let zero = C.Var.fresh () in
  let failure = C.Var.fresh () in
  let value = C.Var.fresh () in
  let exception_ = C.Var.fresh_n "handler_exception" in
  let argument = C.Var.fresh () in
  let thunk = C.Var.fresh () in
  let captured, body_exit, handler_exit =
    match kind with
    | `Try_body -> value, C.Poptrap (3, []), C.Raise (exception_, `Reraise)
    | `Handler -> exception_, C.Raise (failure, `Normal), C.Branch (3, [])
    | `Ordinary_handler_entry -> exception_, C.Poptrap (2, []), C.Branch (3, [])
  in
  let program_blocks =
    blocks
      [ ( 0
        , block
            [ C.Let (zero, Constant (Int Jsoo.Targetint.zero))
            ; C.Let (failure, Prim (Extern ("late_test_exception", None), [ Pv zero ]))
            ]
            (Pushtrap ((1, []), exception_, (2, []))) )
      ; ( 1
        , block
            [ C.Let (value, Prim (Extern ("late_test_probe", None), [ Pv zero ])) ]
            body_exit )
      ; 2, block [] handler_exit
      ; ( 3
        , block
            [ C.Let (thunk, Closure ([ argument ], (4, []), (None, None))); anchor thunk ]
            Stop )
      ; 4, block [ sink captured ] (Return argument)
      ]
  in
  { C.start = 0; free_pc = 5; blocks = program_blocks }

let print_exception_capture kind =
  match Late.split_exn (exception_capture_program kind) with
  | split ->
      Printf.printf
        "captures: [%s]; program blocks: %d\n"
        (definitions (Late.report split).captures)
        (Late.report split).globally_optimized_blocks
  | exception Failure message -> print_endline ("Error: " ^ message)

let%expect_test "capture a try-body value when only the try body reaches the anchor" =
  print_exception_capture `Try_body;
  [%expect {| captures: [primitive]; program blocks: 5 |}]

let%expect_test "capture the exception when only the handler reaches the anchor" =
  print_exception_capture `Handler;
  [%expect {| captures: [control-flow value]; program blocks: 5 |}]

let%expect_test "a handler target also reached normally does not bind the exception" =
  print_exception_capture `Ordinary_handler_entry;
  [%expect
    {| Error: Crossing value 4 (handler_exception) does not dominate the split anchor |}]
