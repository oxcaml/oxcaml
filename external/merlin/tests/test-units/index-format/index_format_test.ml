open Merlin_index_format
module Facts = Module_implementation_facts

let uid name =
  Shape.Uid.of_compilation_unit_name (Compilation_unit.Name.of_string name)

let compilation_unit = Compilation_unit.of_string "Roundtrip"

let position file line column =
  { Lexing.pos_fname = file;
    pos_lnum = line;
    pos_bol = 40 * line;
    pos_cnum = (40 * line) + column
  }

let span ~ghost ~file ~line =
  { Location.loc_start = position file line 2;
    loc_end = position file line 17;
    loc_ghost = ghost
  }

(* Exercises every context, key, node, kind and reason constructor, so that a
   missing case in the codec shows up as a roundtrip failure. *)
let sample_facts () =
  let deep_context : Facts.Context.t =
    Proj
      ( App
          ( Proj (Body (uid "Owner"), uid "Functor"),
            App
              ( Site (compilation_unit, Interface, 3),
                Site (compilation_unit, Implementation, 3) ) ),
        uid "Member" )
  in
  Facts.normalize
    { checks =
        [ { expectation = Named (Def (uid "Unit"), uid "Expected");
            implementation = Uid (uid "Implementation");
            kind = Ascription;
            site = span ~ghost:false ~file:"site.ml" ~line:3
          };
          { expectation = Named (deep_context, uid "Origin");
            implementation =
              Location
                (compilation_unit, span ~ghost:false ~file:"node.ml" ~line:11);
            kind = Argument;
            site = span ~ghost:true ~file:"ghost.ml" ~line:19
          };
          { expectation = Anon (uid "Package_expectation");
            implementation = Uid (uid "Packed");
            kind = Package;
            site = Location.none
          };
          { expectation = Anon (uid "Interface_expectation");
            implementation = Uid (uid "Impl_member");
            kind = Interface;
            site = Location.none
          }
        ];
      dependencies =
        List.mapi
          (fun index reason : Facts.Dependency.t ->
            { derived =
                Named
                  ( Def (uid (Printf.sprintf "Derived%d" index)),
                    uid "Derived_decl" );
              source = Anon (uid "Source");
              reason
            })
          [ Facts.Dependency.Reason.Definition;
            Alias;
            Include;
            With_constraint;
            Destructive_substitution;
            Module_type_of;
            Strengthening;
            Functor_type;
            Instance;
            Argument_member;
            Interface
          ];
      equalities =
        [ { left = Def (uid "Binding");
            right = App (Def (uid "Functor"), Def (uid "Arg"))
          }
        ];
      omissions =
        List.map
          (fun (affected, source, reason) : Facts.Omission.t ->
            { affected; source; reason })
          [ ( Some (Facts.Key.Named (Body (uid "F"), uid "S")),
              Some (uid "Source_family"),
              Facts.Omission.Reason.Unresolved_module_type );
            (None, Some (uid "Lone_source"), Unresolved_module);
            (Some (Anon (uid "Lone_affected")), None, Unsupported_path);
            (None, None, Missing_parameter_expectation)
          ]
    }

let other_facts () =
  Facts.normalize
    { checks =
        [ { expectation = Named (Def (uid "Other"), uid "Other_S");
            implementation = Uid (uid "Other_impl");
            kind = Ascription;
            site = Location.none
          }
        ];
      dependencies =
        [ { derived = Anon (uid "Other_derived");
            source = Named (Def (uid "Other"), uid "Other_S");
            reason = Alias
          }
        ];
      equalities = [];
      omissions = []
    }

let index_of_facts ~facts ~present : Index_format.index =
  { defs = Index_format.Uid_map.empty ();
    approximated = Index_format.Uid_map.empty ();
    cu_shape = Hashtbl.create 1;
    stats = Index_format.Stats.empty;
    root_directory = None;
    related_uids = Index_format.Uid_map.empty ();
    module_facts =
      if present then Some (Index_format.inline_module_facts facts) else None
  }

let facts_of_index (index : Index_format.index) =
  match index.module_facts with
  | None -> Alcotest.fail "the index does not contain module facts"
  | Some module_facts -> (
    match
      Module_facts_compact.to_facts
        (Index_format.module_facts_block module_facts)
    with
    | Ok facts -> facts
    | Error message ->
      Alcotest.failf "cannot decode the written facts: %s" message)

let check_facts message expected actual =
  Alcotest.check Alcotest.int message 0 (Facts.compare expected actual)

let check_malformed message block =
  match Module_facts_compact.to_facts block with
  | Ok _ -> Alcotest.failf "%s: the block was accepted" message
  | Error _ -> ()

(* Index files are only ever read back through their path, so the tests write
   real (uniquely named) files and delete them afterwards. *)
let with_files count f =
  let files =
    List.init count (fun index ->
        Filename.temp_file
          (Printf.sprintf "merlin-facts-%d-" index)
          ".ocaml-index")
  in
  Fun.protect
    ~finally:(fun () ->
      List.iter (fun file -> try Sys.remove file with Sys_error _ -> ()) files)
    (fun () -> f files)

let write_facts ~file ?(present = true) facts =
  Index_format.write ~file (index_of_facts ~facts ~present)

let test_codec_roundtrip =
  Alcotest.test_case "the compact codec roundtrips every fact" `Quick (fun () ->
      let facts = sample_facts () in
      let block = Module_facts_compact.of_facts facts in
      Alcotest.check Alcotest.bool "a populated block is not empty" false
        (Module_facts_compact.is_empty block);
      match Module_facts_compact.to_facts block with
      | Error message -> Alcotest.failf "decoding failed: %s" message
      | Ok decoded ->
        check_facts "the decoded facts are the encoded ones" facts decoded)

let test_empty_roundtrip =
  Alcotest.test_case "the empty block roundtrips to the empty facts" `Quick
    (fun () ->
      Alcotest.check Alcotest.bool "the empty block is empty" true
        (Module_facts_compact.is_empty Module_facts_compact.empty);
      match Module_facts_compact.to_facts Module_facts_compact.empty with
      | Error message -> Alcotest.failf "decoding failed: %s" message
      | Ok decoded -> check_facts "no facts" Facts.empty decoded)

let test_malformed_blocks =
  Alcotest.test_case "malformed blocks are rejected rather than decoded" `Quick
    (fun () ->
      let block = Module_facts_compact.of_facts (sample_facts ()) in
      check_malformed "a future version"
        { block with version = block.version + 1 };
      check_malformed "a truncated check table" { block with checks = "" };
      check_malformed "a truncated context table" { block with contexts = "" };
      check_malformed "a missing uid table" { block with uids = [||] };
      check_malformed "a missing compilation unit table"
        { block with units = [||] };
      check_malformed "a missing file table" { block with files = [||] };
      check_malformed "trailing bytes after the omissions"
        { block with omissions = block.omissions ^ "\x00" };
      check_malformed "a context count larger than the table"
        { block with context_count = block.context_count + 1 };
      check_malformed "a key count larger than the table"
        { block with key_count = block.key_count + 1 })

let test_malformed_integers =
  Alcotest.test_case "integers must use their canonical encoding" `Quick
    (fun () ->
      let decode payload =
        Module_facts_compact.For_testing.decode_canonical_uint
          ~max_native_int:(Int64.of_int max_int) payload
      in
      (match decode "\x01" with
      | Ok value ->
        Alcotest.check Alcotest.string "one" "1" (Int64.to_string value)
      | Error message -> Alcotest.failf "1 was rejected: %s" message);
      let reject message payload =
        match decode payload with
        | Ok _ -> Alcotest.failf "%s: the encoding was accepted" message
        | Error _ -> ()
      in
      reject "a redundant continuation byte" "\x80\x00";
      reject "a truncated integer" "\x80";
      reject "trailing bytes" "\x01\x01";
      reject "an integer that overflows 64 bits"
        "\xff\xff\xff\xff\xff\xff\xff\xff\xff\x7f")

let test_writer_roundtrip =
  Alcotest.test_case "index files carry their facts across write and read"
    `Quick (fun () ->
      with_files 2 (fun files ->
          let present_file = List.nth files 0 in
          let absent_file = List.nth files 1 in
          let facts = sample_facts () in
          write_facts ~file:present_file facts;
          write_facts ~file:absent_file ~present:false facts;
          let present = Index_format.read_exn ~file:present_file in
          let absent = Index_format.read_exn ~file:absent_file in
          Alcotest.check Alcotest.bool "present facts stay present" true
            (Option.is_some present.module_facts);
          Alcotest.check Alcotest.bool "absent facts stay absent" true
            (Option.is_none absent.module_facts);
          check_facts "the facts survive the roundtrip" facts
            (facts_of_index present)))

let test_reader_status =
  Alcotest.test_case "the reader reports what it could and could not read"
    `Quick (fun () ->
      with_files 4 (fun files ->
          let good = List.nth files 0 in
          let absent = List.nth files 1 in
          let missing = List.nth files 2 in
          let malformed = List.nth files 3 in
          let facts = sample_facts () in
          let more_facts = other_facts () in
          write_facts ~file:good facts;
          write_facts ~file:absent ~present:false more_facts;
          Sys.remove missing;
          Index_format.write ~file:malformed
            { (index_of_facts ~facts:Facts.empty ~present:true) with
              module_facts =
                Some
                  (Index_format.link_module_facts
                     { Module_facts_compact.empty with version = 99 })
            };
          let loaded, status = Module_facts_reader.load ~index_files:[ good ] in
          check_facts "a single index loads its facts" facts loaded;
          Alcotest.check Alcotest.bool "the answer is complete" true
            status.facts_present;
          Alcotest.check Alcotest.int "one channel was loaded" 1
            status.channels_loaded;
          Alcotest.check Alcotest.int "one source was folded" 1
            status.sources_folded;
          Alcotest.check Alcotest.int "nothing went wrong" 0
            (List.length status.problems);
          let loaded, status =
            Module_facts_reader.load ~index_files:[ good; absent ]
          in
          check_facts "absent channels do not contribute facts" facts loaded;
          Alcotest.check Alcotest.bool "an absent channel taints the answer"
            false status.facts_present;
          Alcotest.check Alcotest.int "one channel was loaded" 1
            status.channels_loaded;
          let loaded, status =
            Module_facts_reader.load ~index_files:[ good; missing ]
          in
          check_facts "the readable facts are kept" facts loaded;
          Alcotest.check Alcotest.bool "a missing file taints the answer" false
            status.facts_present;
          (match status.problems with
          | [ Unreadable _ ] -> ()
          | problems ->
            Alcotest.failf "expected one unreadable file, got %d"
              (List.length problems));
          let loaded, status =
            Module_facts_reader.load ~index_files:[ good; malformed ]
          in
          check_facts "malformed facts do not poison the good ones" facts loaded;
          Alcotest.check Alcotest.bool "a malformed block taints the answer"
            false status.facts_present;
          (match status.problems with
          | [ Malformed _ ] -> ()
          | problems ->
            Alcotest.failf "expected one malformed file, got %d"
              (List.length problems));
          let paths, status =
            Module_facts_reader.fold ~index_files:[ good; absent ] ~init:[]
              ~f:(fun paths ~path (_ : Facts.t) ->
                Filename.basename path :: paths)
          in
          Alcotest.check Alcotest.int "the present source is visited once" 1
            (List.length paths);
          Alcotest.check Alcotest.int "one source was folded" 1
            status.sources_folded;
          let count, status =
            Module_facts_reader.fold ~index_files:[] ~init:0
              ~f:(fun count ~path:_ (_ : Facts.t) -> count + 1)
          in
          Alcotest.check Alcotest.int "nothing to fold" 0 count;
          Alcotest.check Alcotest.bool
            "no configured index is a complete answer" true status.facts_present))

let () =
  Alcotest.run "merlin-lib.index_format"
    [ ( "module facts",
        [ test_codec_roundtrip;
          test_empty_roundtrip;
          test_malformed_blocks;
          test_malformed_integers;
          test_writer_roundtrip;
          test_reader_status
        ] )
    ]
