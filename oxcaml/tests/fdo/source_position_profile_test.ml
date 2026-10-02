(* Round-trip and validation tests for [Source_position_profile]. *)
module P = Source_position_profile
module F = Fdo_counter

let failures = ref 0

let check name cond =
  if not cond
  then (
    incr failures;
    Printf.eprintf "FAILED: %s\n%!" name)

let check_count name actual expected =
  if not (Int64.equal actual expected)
  then (
    incr failures;
    Printf.eprintf "FAILED: %s: got %Ld, expected %Ld\n%!" name actual expected)

let fn unmangled_name = F.function_id ~unmangled_name ~discriminator:0

let pos function_id function_body_hash ast_pos edge =
  F.position ~function_id ~function_body_hash ~ast_pos ~edge

let counter position inlining_stack : F.t = { position; inlining_stack }

let leaf_a = fn "A.f"

(* Body hashes: [body_a] is the body of [leaf_a] and [leaf_c] has [body_c];
   [body_edited] stands for any other body. *)
let body_hash n = F.Function_body_hash.of_int32 (Int32.of_int n)

let body_a = body_hash 0xa

let body_b = body_hash 0xb

let body_c = body_hash 0xc

let body_d = body_hash 0xd

let body_e = body_hash 0xe

let body_edited = body_hash 0xed

let leaf_c = fn "C.f"

let ctx_b = pos (fn "B.g") body_b 3 F.Callsite

let ctx_d = pos (fn "D.g") body_d 4 F.Callsite

let edge_a edge = pos leaf_a body_a 7 edge

let add_entry w fn context count =
  P.Writer.add_counter w ~counter:(counter (F.function_entry fn) context) ~count

let add_position w position context count =
  P.Writer.add_counter w ~counter:(counter position context) ~count

let count_entry p fn context =
  P.recorded_count p (counter (F.function_entry fn) context)

let count_position p position context =
  P.recorded_count p (counter position context)

let check_bound name (actual : P.bound) ~lower ~upper ~estimate =
  let string_of_bound (b : P.bound) =
    Printf.sprintf "%g..%g%s" b.lower b.upper
      (match b.estimate with
      | Some e -> Printf.sprintf " (estimate %g)" e
      | None -> "")
  in
  if
    not
      (Float.equal actual.lower lower
      && Float.equal actual.upper upper
      && Option.equal Float.equal actual.estimate estimate)
  then (
    incr failures;
    Printf.eprintf "FAILED: %s: got %s, expected %s\n%!" name
      (string_of_bound actual)
      (string_of_bound { lower; upper; estimate }))

(* Golden hashes pin the hash function: a change here changes every profile's
   keys. *)
let () =
  let specialized =
    F.specialized ~unspecialized:leaf_a ~specialization_site:ctx_b
  in
  check "canonical function id"
    (String.equal (F.function_id_to_string leaf_a) "\"A.f\":0");
  check "canonical position"
    (String.equal
       (F.position_to_string (edge_a Then))
       "\"A.f\":0:0000000a:7:then");
  check "canonical specialization"
    (String.equal
       (F.function_id_to_string specialized)
       "\"A.f\":0@(\"B.g\":0:0000000b:3:call)");
  check "golden function hash"
    (F.Hash.equal (F.hash_function_id leaf_a) (F.Hash.of_int32 0x48c07ea9l));
  check "golden position hash"
    (F.Hash.equal (F.hash_position (edge_a Then)) (F.Hash.of_int32 0xd9a220fal));
  check "golden specialized function hash"
    (F.Hash.equal
       (F.hash_function_id specialized)
       (F.Hash.of_int32 0x751e0315l));
  check "function and position hashes have distinct tags"
    (F.Hash.is_function_entry (F.hash_function_id specialized)
    && not (F.Hash.is_function_entry (F.hash_position ctx_b)));
  check "hash unsigned ordering"
    (F.Hash.compare (F.Hash.of_int32 0x80000000l) (F.Hash.of_int32 0x7fffffffl)
    > 0);
  check "hash hexadecimal format"
    (String.equal
       (Printf.sprintf "%08lx" (F.Hash.of_int32 0xffffffffl :> int32))
       "ffffffff");
  let edges : F.edge list =
    [Then; Else; Switch_case 0; Switch_case 1; Callsite]
  in
  check "edge kinds are distinct"
    (List.length
       (List.sort_uniq F.Hash.compare
          (List.map (fun e -> F.hash_position (edge_a e)) edges))
    = 5);
  check "discriminator distinguishes same-name functions"
    (not
       (F.Hash.equal
          (F.hash_function_id leaf_a)
          (F.hash_function_id
             (F.function_id ~unmangled_name:"A.f" ~discriminator:1))));
  check "interior identity rejects a body edit"
    (not
       (F.Hash.equal
          (F.hash_position (edge_a Then))
          (F.hash_position (pos leaf_a body_edited 7 Then))));
  check "specialization distinguishes call sites"
    (not
       (F.Hash.equal
          (F.hash_function_id specialized)
          (F.hash_function_id
             (F.specialized ~unspecialized:leaf_a ~specialization_site:ctx_d))));
  check "specialization site body is part of identity"
    (not
       (F.Hash.equal
          (F.hash_function_id specialized)
          (F.hash_function_id
             (F.specialized ~unspecialized:leaf_a
                ~specialization_site:(pos (fn "B.g") body_edited 3 Callsite)))))

(* Named module sites have no body hash, including when their initializer was
   itself specialized. Ordinary AST specializations remain hash-guarded. *)
let () =
  let site module_path discriminator =
    F.instantiation_site
      (F.function_id ~unmangled_name:module_path ~discriminator)
  in
  let inner = site "Example.Inner" 0 and outer = site "Example.Outer" 0 in
  let inner_entry =
    F.function_entry
      (F.specialized ~unspecialized:leaf_a ~specialization_site:inner)
  in
  check "named instantiation"
    (F.equal
       (F.specialize
          (counter (F.function_entry leaf_a) [])
          ~at:(counter inner []))
       (counter inner_entry []));
  check "named site has no body hash"
    (String.equal (F.position_to_string inner) "module(\"Example.Inner\":0)");
  check "named site is not a function-entry profile key"
    (not (F.Hash.is_function_entry (F.hash_position inner)));
  check "instantiated entry keeps the entry tag"
    (F.Hash.is_function_entry (F.hash_position inner_entry));
  check "module-site discriminator matters"
    (not
       (F.Hash.equal (F.hash_position inner)
          (F.hash_position (site "Example.Inner" 1))));
  let copied_site = F.specialize (counter inner []) ~at:(counter outer []) in
  let nested_entry =
    F.function_entry
      (F.specialized ~unspecialized:leaf_a
         ~specialization_site:
           (F.instantiation_site
              (F.specialized
                 ~unspecialized:
                   (F.function_id ~unmangled_name:"Example.Inner"
                      ~discriminator:0)
                 ~specialization_site:outer)))
  in
  check "copied module initializer preserves both sites"
    (F.equal
       (F.specialize (counter (F.function_entry leaf_a) []) ~at:copied_site)
       (counter nested_entry []));
  let position hash =
    F.specialize (counter (pos leaf_a hash 7 Then) []) ~at:(counter inner [])
  in
  check "instantiated interior counters still depend on their body"
    (not
       (List.equal F.Hash.equal
          (F.hash (position body_a))
          (F.hash (position body_edited))))

(* A: ten context-free samples plus five inlined at B. C: seven samples inlined
   at B, itself inlined at D. The contexts here are call sites. *)
let make_writer () =
  let w = P.Writer.create () in
  P.Writer.add_body w
    ~hash:(F.hash_function_id leaf_a)
    ~function_body_hash:body_a;
  P.Writer.add_body w
    ~hash:(F.hash_function_id leaf_c)
    ~function_body_hash:body_c;
  add_entry w leaf_a [] 10L;
  add_entry w leaf_a [ctx_b] 5L;
  add_entry w leaf_c [ctx_b; ctx_d] 7L;
  P.Writer.add_hashed_stack w ~hashes:[] ~count:100L;
  w

let check_queries name p =
  let check_count what = check_count (name ^ ": " ^ what) in
  check_count "root sums contexts" (count_entry p leaf_a []) 15L;
  check_count "context refines count" (count_entry p leaf_a [ctx_b]) 5L;
  check_count "unrecorded context" (count_entry p leaf_a [ctx_d]) 0L;
  check_count "deep stack" (count_entry p leaf_c [ctx_b; ctx_d]) 7L;
  check_count "prefix of deep stack" (count_entry p leaf_c [ctx_b]) 7L;
  check_count "context-only position is not a root"
    (count_position p ctx_b [])
    0L;
  check_count "unknown function" (count_entry p (fn "Missing.f") []) 0L;
  let bounds fn context = P.count p (counter (F.function_entry fn) context) in
  let check_bound what = check_bound (name ^ ": " ^ what) in
  check_bound "root is exact" (bounds leaf_a []) ~lower:15. ~upper:15.
    ~estimate:(Some 15.);
  check_bound
    "paths ending above the context might be its; the estimate assumes they \
     split like the others, all of which are"
    (bounds leaf_a [ctx_b]) ~lower:5. ~upper:15. ~estimate:(Some 15.);
  check_bound "missing level of an unknown function: any path might be its"
    (bounds leaf_a [ctx_d]) ~lower:0. ~upper:15. ~estimate:None;
  check_bound "missing level of a same-body function: none is its"
    (bounds leaf_c [ctx_b; pos leaf_a body_a 9 F.Callsite])
    ~lower:0. ~upper:0. ~estimate:(Some 0.);
  check_bound "full context with nothing lost"
    (bounds leaf_c [ctx_b; ctx_d])
    ~lower:7. ~upper:7. ~estimate:(Some 7.);
  check_bound "deeper recorded context refines an exact count"
    (bounds leaf_c [ctx_b]) ~lower:7. ~upper:7. ~estimate:(Some 7.);
  check_bound "unrecorded position of a same-body function never ran"
    (P.count p (counter (pos leaf_a body_a 7 Then) []))
    ~lower:0. ~upper:0. ~estimate:(Some 0.);
  check_bound "unrecorded position of a changed function is unknown"
    (P.count p (counter (pos leaf_a body_edited 7 Then) []))
    ~lower:0. ~upper:infinity ~estimate:None;
  check_bound "unknown function"
    (bounds (fn "Missing.f") [])
    ~lower:0. ~upper:infinity ~estimate:None;
  let status fn hash =
    P.body_status p ~function_id:fn ~function_body_hash:hash
  in
  check "same body" (status leaf_a body_a = P.Same_body);
  check "changed body" (status leaf_a body_edited = P.Changed_body);
  check "unknown function body"
    (status (fn "Missing.f") body_a = P.Unknown_function);
  let bodies = ref [] in
  P.iter_bodies p ~f:(fun ~hash ~function_body_hash ->
      bodies := (hash, function_body_hash) :: !bodies);
  check "body index round-trips in hash order"
    (List.rev !bodies
    = List.sort
        (fun (a, _) (b, _) -> F.Hash.compare a b)
        [F.hash_function_id leaf_a, body_a; F.hash_function_id leaf_c, body_c])

let with_temp_file f =
  let filename = Filename.temp_file "source_position_profile" ".fdo" in
  Fun.protect ~finally:(fun () -> Sys.remove filename) (fun () -> f filename)

let () =
  check_queries "in-memory" (P.Writer.to_profile (make_writer ()));
  with_temp_file (fun filename ->
      P.Writer.write (make_writer ()) ~filename;
      check_queries "file round-trip" (P.load ~filename));
  with_temp_file (fun filename ->
      P.Writer.write (make_writer ()) ~filename;
      let mapped = ref false in
      P.register_mmap (fun filename ->
          mapped := true;
          let contents =
            In_channel.with_open_bin filename In_channel.input_all
          in
          Bigarray.Array1.init Bigarray.char Bigarray.c_layout
            (String.length contents) (String.get contents));
      check_queries "mapped round-trip" (P.load ~filename);
      check "registered mapper used" !mapped);
  with_temp_file (fun filename ->
      P.Writer.write (P.Writer.create ()) ~filename;
      let p = P.load ~filename in
      check_count "empty profile query" (count_entry p leaf_a []) 0L;
      check "empty profile knows no bodies"
        (P.body_status p ~function_id:leaf_a ~function_body_hash:body_a
        = P.Unknown_function);
      check "empty call targets" (List.is_empty (P.call_targets p ctx_b)));
  check "conflicting body hashes are rejected"
    (match
       let w = P.Writer.create () in
       P.Writer.add_body w
         ~hash:(F.hash_function_id leaf_a)
         ~function_body_hash:body_a;
       P.Writer.add_body w
         ~hash:(F.hash_function_id leaf_a)
         ~function_body_hash:body_c
     with
    | () -> false
    | exception Invalid_argument _ -> true)

(* With a second context for A (15 samples at E), the 10 context-free samples
   are estimated to split 1:3 between B and E like the recorded ones. *)
let () =
  let w = make_writer () in
  let ctx_e = pos (fn "E.g") body_e 5 F.Callsite in
  add_entry w leaf_a [ctx_e] 15L;
  let p = P.Writer.to_profile w in
  let bounds context = P.count p (counter (F.function_entry leaf_a) context) in
  check_bound "lost paths split like the continuing ones: smaller share"
    (bounds [ctx_b]) ~lower:5. ~upper:15. ~estimate:(Some 7.5);
  check_bound "lost paths split like the continuing ones: larger share"
    (bounds [ctx_e]) ~lower:15. ~upper:25. ~estimate:(Some 22.5);
  check_bound "missing level of an unknown function under both" (bounds [ctx_d])
    ~lower:0. ~upper:30. ~estimate:None

let () =
  let w = make_writer () in
  add_position w (edge_a Then) [ctx_b] 5L;
  let check_targets name p =
    check
      (name ^ ": call targets exclude branch roots")
      (List.equal F.Hash.equal
         (List.sort F.Hash.compare (P.call_targets p ctx_b))
         (List.sort F.Hash.compare
            [F.hash_function_id leaf_a; F.hash_function_id leaf_c]));
    check
      (name ^ ": unknown call site")
      (List.is_empty (P.call_targets p ctx_d));
    let count root context =
      P.count_for_deepest_context p ~root:(F.hash_function_id root)
        ~context:(List.map F.hash_position context)
    in
    check_count (name ^ ": context refines") (count leaf_a [ctx_b]) 5L;
    check_count
      (name ^ ": deeper context aggregates")
      (count leaf_a [ctx_b; ctx_d])
      5L;
    check_count
      (name ^ ": unknown context aggregates")
      (count leaf_a [ctx_d]) 15L;
    check_count (name ^ ": full context") (count leaf_c [ctx_b; ctx_d]) 7L;
    check_count (name ^ ": unknown root") (count (fn "Missing.f") [ctx_b]) 0L;
    let pairs = ref [] in
    P.iter_call_targets p ~f:(fun ~callsite ~callee ->
        pairs := (callsite, callee) :: !pairs);
    check
      (name ^ ": call-target index")
      (List.sort compare !pairs
      = List.sort compare
          [ F.hash_position ctx_b, F.hash_function_id leaf_a;
            F.hash_position ctx_b, F.hash_function_id leaf_c ])
  in
  check_targets "memory" (P.Writer.to_profile w);
  with_temp_file (fun filename ->
      P.Writer.write w ~filename;
      check_targets "file" (P.load ~filename))

let () =
  let w = make_writer () in
  add_position w (edge_a (Switch_case 3)) [] 40L;
  add_position w (edge_a (Switch_case 4)) [] 20L;
  add_position w (edge_a (Switch_case 3)) [ctx_b] 10L;
  let check_edges name p =
    let check_count what = check_count (name ^ ": " ^ what) in
    check_count "edge aggregates contexts"
      (count_position p (edge_a (Switch_case 3)) [])
      50L;
    check_count "other edge" (count_position p (edge_a (Switch_case 4)) []) 20L;
    check_count "context refines"
      (count_position p (edge_a (Switch_case 3)) [ctx_b])
      10L;
    check_count "unknown context"
      (count_position p (edge_a (Switch_case 3)) [ctx_d])
      0L;
    check_count "unknown edge" (count_position p (edge_a (Switch_case 9)) []) 0L;
    check_count "entry retained" (count_entry p leaf_a []) 15L;
    check_count "changed body has no interior counts"
      (count_position p (pos leaf_a body_edited 7 (Switch_case 3)) [])
      0L
  in
  check_edges "memory" (P.Writer.to_profile w);
  with_temp_file (fun filename ->
      P.Writer.write w ~filename;
      check_edges "file" (P.load ~filename));
  P.Writer.add_hashed_stack w
    ~hashes:[F.hash_position (edge_a (Switch_case 3)); F.hash_position ctx_b]
    ~count:5L;
  check_count "hashed stack reaches the same nodes"
    (count_position (P.Writer.to_profile w) (edge_a (Switch_case 3)) [ctx_b])
    15L

(* More than eight keys exercises integer interpolation, including unsigned
   boundaries, clustered keys and absent keys in gaps. *)
let () =
  let keys =
    List.map F.Hash.of_int32
      [ 0l;
        1l;
        2l;
        0x70000000l;
        0x7ffffffel;
        0x7fffffffl;
        0x80000000l;
        0x80000001l;
        0xfffffffel;
        0xffffffffl ]
  in
  let writer = P.Writer.create () in
  List.iteri
    (fun i root ->
      P.Writer.add_hashed_stack writer ~hashes:[root]
        ~count:(Int64.of_int (i + 1)))
    keys;
  let profile = P.Writer.to_profile writer in
  List.iteri
    (fun i root ->
      check_count "integer interpolation hit"
        (P.count_for_deepest_context profile ~root ~context:[])
        (Int64.of_int (i + 1)))
    keys;
  List.iter
    (fun root ->
      check_count "integer interpolation miss"
        (P.count_for_deepest_context profile ~root:(F.Hash.of_int32 root)
           ~context:[])
        0L)
    [3l; 0x70000001l; 0x80000002l; 0xfffffffdl]

let () =
  let expect_error name contents =
    with_temp_file (fun filename ->
        Out_channel.with_open_bin filename (fun oc ->
            Out_channel.output_string oc contents);
        match
          let p = P.load ~filename in
          P.iter p ~f:(fun ~hash:_ ~depth:_ ~count:_ ~ending:_ -> ())
        with
        | () -> check name false
        | exception P.Error _ -> ())
  in
  let good =
    with_temp_file (fun filename ->
        P.Writer.write (make_writer ()) ~filename;
        In_channel.with_open_bin filename In_channel.input_all)
  in
  let magic_len = String.length P.magic_number in
  expect_error "empty file" "";
  expect_error "truncated magic" (String.sub good 0 (magic_len - 1));
  expect_error "truncated header" (String.sub good 0 (magic_len + 3));
  expect_error "truncated body" (String.sub good 0 (String.length good - 1));
  expect_error "trailing bytes" (good ^ "x");
  expect_error "wrong magic" ("X" ^ String.sub good 1 (String.length good - 1));
  let wrong_version = Bytes.of_string good in
  Bytes.set wrong_version (magic_len - 1) '\255';
  expect_error "wrong version" (Bytes.to_string wrong_version);
  Bytes.set wrong_version (magic_len - 1) '\011';
  expect_error "profile version with single node counts"
    (Bytes.to_string wrong_version)

let () =
  if !failures > 0
  then (
    Printf.eprintf "%d test(s) failed\n%!" !failures;
    exit 1)
  else print_endline "All tests passed"
