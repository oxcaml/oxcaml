open! Js_of_ocaml_compiler.Stdlib
module Partition = Jsoo_static_partition.Partition

let unit_ ?(provides = []) ?(requires = []) ?(imports = []) ?(force_link = false) name :
    Partition.Unit.t =
  { name; provides; requires; imports; force_link }

let print_json json = print_endline (Yojson.Safe.to_string json)

let print_result f =
  match f () with
  | json -> print_json json
  | exception Failure message -> print_endline ("Error: " ^ message)

let print units =
  print_result (fun () ->
      Partition.Report.to_json
        (Partition.report (Partition.create units ~main_unit:"Main" ~lazy_unit:"Lazy")))

let%expect_test "shared closure, unique dependencies, force-link roots, and unused inputs"
    =
  print
    [ unit_ "Shared"
    ; unit_ ~requires:[ "Shared" ] "Side_effect_dependency"
    ; unit_ ~force_link:true ~requires:[ "Side_effect_dependency" ] "Side_effect"
    ; unit_ ~requires:[ "Shared" ] "Main"
    ; unit_ "Lazy_dependency"
    ; unit_ ~requires:[ "Shared"; "Lazy_dependency" ] "Lazy"
    ; unit_ "Unused"
    ];
  [%expect
    {| {"eager":["Shared","Side_effect_dependency","Side_effect","Main"],"lazy":["Lazy_dependency","Lazy"],"shared":["Shared"],"forced_eager":["Side_effect"],"unused":["Unused"],"main_exports":["Shared"]} |}]

let%expect_test "preserve input order, not lexical order" =
  print
    [ unit_ "Z_dependency"
    ; unit_ "A_dependency"
    ; unit_ ~requires:[ "Z_dependency"; "A_dependency" ] "Main"
    ; unit_ "Lazy"
    ];
  [%expect
    {| {"eager":["Z_dependency","A_dependency","Main"],"lazy":["Lazy"],"shared":[],"forced_eager":[],"unused":[],"main_exports":[]} |}]

let%expect_test "packed-unit aliases are indivisible; only boundary names escape" =
  print
    [ unit_ ~provides:[ "Packed.A"; "Packed.B" ] ~requires:[ "Packed.B" ] "Packed"
    ; unit_ ~requires:[ "Packed.A" ] "Main"
    ; unit_ ~requires:[ "Packed.B" ] "Lazy"
    ];
  [%expect
    {| {"eager":["Packed","Main"],"lazy":["Lazy"],"shared":["Packed"],"forced_eager":[],"unused":[],"main_exports":["Packed.B"]} |}]

let%expect_test "interface imports are not runtime dependencies" =
  print
    [ unit_ ~imports:[ "Interface_only", Some "same" ] "Main"
    ; unit_ ~imports:[ "Interface_only", Some "same" ] "Lazy"
    ];
  [%expect
    {| {"eager":["Main"],"lazy":["Lazy"],"shared":[],"forced_eager":[],"unused":[],"main_exports":[]} |}]

let%expect_test "duplicate providers, including packed aliases" =
  print [ unit_ "Main"; unit_ "Main"; unit_ "Lazy" ];
  [%expect
    {| Error: Duplicate compilation-unit provider Main (provided by Main and Main) |}];
  print [ unit_ ~provides:[ "Alias" ] "Main"; unit_ ~provides:[ "Alias" ] "Lazy" ];
  [%expect
    {| Error: Duplicate compilation-unit provider Alias (provided by Main and Lazy) |}]

let%expect_test "inconsistent interfaces" =
  print
    [ unit_ ~imports:[ "Interface", Some "first" ] "Main"
    ; unit_ ~imports:[ "Interface", Some "second" ] "Lazy"
    ];
  [%expect {| Error: Inconsistent interface imports of Interface (by Main and Lazy) |}]

let%expect_test "unresolved root and dependency" =
  print [ unit_ "Main" ];
  [%expect
    {| Error: Unresolved compilation-unit dependency Lazy (required by lazy root) |}];
  print [ unit_ "Main"; unit_ ~requires:[ "Missing" ] "Lazy" ];
  [%expect {| Error: Unresolved compilation-unit dependency Missing (required by Lazy) |}]

let%expect_test "cycles and ownership conflicts" =
  print
    [ unit_ "Main"
    ; unit_ ~requires:[ "Lazy" ] "Dependency"
    ; unit_ ~requires:[ "Dependency" ] "Lazy"
    ];
  [%expect {| Error: Cyclic compilation-unit dependencies: Lazy -> Dependency -> Lazy |}];
  print [ unit_ "Lazy"; unit_ ~requires:[ "Lazy" ] "Main" ];
  [%expect
    {| Error: Lazy root Lazy is required by the eager partition (including force-linked roots) |}];
  print [ unit_ "Main"; unit_ ~force_link:true "Lazy" ];
  [%expect
    {| Error: Lazy root Lazy is required by the eager partition (including force-linked roots) |}]

let print_many ?(require_deferred_units = []) units ~lazy_units =
  print_result (fun () ->
      let partition =
        Partition.create_many units ~main_unit:"Main" ~lazy_units ~require_deferred_units
      in
      `List
        [ Partition.Report.to_json (Partition.report partition)
        ; `Assoc
            (List.map (Partition.lazy_roots partition) ~f:(fun (id, units) ->
                 ( id
                 , `List
                     (List.map units ~f:(fun (unit_ : Partition.Unit.t) ->
                          `String unit_.name)) )))
        ])

let%expect_test "three independent roots share ordinary runtime dependencies eagerly" =
  let units =
    [ unit_ "Shared"
    ; unit_ ~requires:[ "Shared" ] "Only_lazy_shared"
    ; unit_ "Main"
    ; unit_ ~requires:[ "Only_lazy_shared" ] "A"
    ; unit_ ~requires:[ "Only_lazy_shared" ] ~imports:[ "A", None ] "B"
    ; unit_ "C"
    ]
  in
  print_many units ~lazy_units:[ "A"; "B"; "C" ];
  [%expect
    {| [{"eager":["Shared","Only_lazy_shared","Main"],"lazy":["A","B","C"],"shared":["Shared","Only_lazy_shared"],"forced_eager":[],"unused":[],"main_exports":["Only_lazy_shared"]},{"A":["A"],"B":["B"],"C":["C"]}] |}];
  print_many units ~lazy_units:[ "C"; "B"; "A" ];
  [%expect
    {| [{"eager":["Shared","Only_lazy_shared","Main"],"lazy":["A","B","C"],"shared":["Shared","Only_lazy_shared"],"forced_eager":[],"unused":[],"main_exports":["Only_lazy_shared"]},{"C":["C"],"B":["B"],"A":["A"]}] |}]

let%expect_test "reject implementation sharing with runtime dependency paths" =
  print_many
    [ unit_ "Main"
    ; unit_ "A_impl"
    ; unit_ ~requires:[ "A_impl" ] "A"
    ; unit_ ~requires:[ "A_impl" ] "Helper"
    ; unit_ ~requires:[ "Helper" ] "B"
    ; unit_ "C"
    ]
    ~require_deferred_units:[ "A_impl" ]
    ~lazy_units:[ "A"; "B"; "C" ];
  [%expect
    {| Error: Imported implementation A_impl must belong to exactly one lazy root; move shared values and types into an ordinary module and use the generated handle (A: A -> A_impl; B: B -> Helper -> A_impl) |}];
  print_many
    [ unit_ "Main"; unit_ ~provides:[ "Packed.A"; "Packed.B" ] "Packed" ]
    ~lazy_units:[ "Packed.A"; "Packed.B" ];
  [%expect
    {| Error: Lazy roots Packed.A and Packed.B share a compilation-unit provider |}]

let%expect_test "rejection paths visit shared dependency diamonds only once" =
  let dependency index = Printf.sprintf "Dependency_%d" index in
  let dependencies =
    List.init ~len:60 ~f:(fun index ->
        let requires =
          match index with
          | 0 -> []
          | 1 -> [ dependency 0 ]
          | _ -> [ dependency (index - 1); dependency (index - 2) ]
        in
        unit_ ~requires (dependency index))
  in
  let units =
    dependencies
    @ [ unit_ ~requires:[ dependency 59 ] "Main"
      ; unit_ "A_impl"
      ; unit_ ~requires:[ dependency 59; "A_impl" ] "A"
      ; unit_ ~requires:[ dependency 59; "A_impl" ] "B"
      ; unit_ ~requires:[ dependency 59 ] "C"
      ]
  in
  print_many units ~require_deferred_units:[ "A_impl" ] ~lazy_units:[ "A"; "B"; "C" ];
  [%expect
    {| Error: Imported implementation A_impl must belong to exactly one lazy root; move shared values and types into an ordinary module and use the generated handle (A: A -> A_impl; B: B -> A_impl) |}]

let%expect_test "handle independence is separate from eager ownership" =
  let run ~handle_requires ~main_requires ~force_implementation =
    let units =
      [ unit_ ~force_link:force_implementation "Impl"
      ; unit_ ~requires:[ "Impl" ] "Helper"
      ; unit_ ~requires:handle_requires ~imports:[ "Impl", None ] "Handle"
      ; unit_ ~requires:main_requires "Main"
      ; unit_ ~requires:[ "Handle"; "Impl" ] "Lazy"
      ]
    in
    print_result (fun () ->
        let partition =
          Partition.create_many
            ~check_deferred_handles:[ "Handle", "Impl" ]
            units
            ~main_unit:"Main"
            ~lazy_units:[ "Lazy" ]
            ~require_deferred_units:[]
        in
        `List (List.map (Partition.report partition).eager ~f:(fun name -> `String name)))
  in
  run ~handle_requires:[] ~main_requires:[ "Handle"; "Impl" ] ~force_implementation:false;
  [%expect {| ["Impl","Handle","Main"] |}];
  run ~handle_requires:[] ~main_requires:[ "Handle" ] ~force_implementation:true;
  [%expect {| ["Impl","Handle","Main"] |}];
  run
    ~handle_requires:[ "Helper" ]
    ~main_requires:[ "Handle"; "Impl" ]
    ~force_implementation:false;
  [%expect
    {| Error: Deferred handle Handle has a runtime dependency on its implementation Impl: Handle -> Helper -> Impl |}];
  print_result (fun () ->
      ignore
        (Partition.create_many
           ~check_deferred_handles:[ "Missing", "Impl" ]
           [ unit_ "Impl"; unit_ "Main"; unit_ "Lazy" ]
           ~main_unit:"Main"
           ~lazy_units:[ "Lazy" ]
           ~require_deferred_units:[]
          : Partition.t);
      `Null);
  [%expect
    {| Error: Unresolved compilation-unit dependency Missing (required by deferred handle) |}]

let%expect_test "reject out-of-order initialization" =
  print [ unit_ ~requires:[ "Dependency" ] "Main"; unit_ "Dependency"; unit_ "Lazy" ];
  [%expect {| Error: Inputs are not in initialization order: Main requires Dependency |}]
