let seed = 0x5eed123

let base_product_case_count = 100

let chain_embedding_case_count = 50

let rng = Random.State.make [| seed; 0x2026; 0x1234 |]

let fail_case kind index source exn =
  Test_support.failf
    "%s case %d failed (seed=%d)\nsource:\n%s\nerror: %s"
    kind
    index
    seed
    source
    (Printexc.to_string exn)

let bool_matrix n value = Array.init n (fun _ -> Array.make n value)

let transitive_closure leq =
  let n = Array.length leq in
  for k = 0 to n - 1 do
    for i = 0 to n - 1 do
      if leq.(i).(k)
      then
        for j = 0 to n - 1 do
          leq.(i).(j) <- leq.(i).(j) || (leq.(i).(k) && leq.(k).(j))
        done
    done
  done

let make_random_poset ji_count =
  let leq = bool_matrix ji_count false in
  for i = 0 to ji_count - 1 do
    leq.(i).(i) <- true
  done;
  for i = 0 to ji_count - 1 do
    for j = i + 1 to ji_count - 1 do
      if Random.State.bool rng then leq.(i).(j) <- true
    done
  done;
  transitive_closure leq;
  leq

let popcount n =
  let rec loop acc n =
    if n = 0 then acc else loop (acc + (n land 1)) (n lsr 1)
  in
  loop 0 n

let ideals_of_poset leq =
  let ji_count = Array.length leq in
  let max_subset = 1 lsl ji_count in
  let ideals = ref [] in
  for subset = 0 to max_subset - 1 do
    let closed = ref true in
    for upper = 0 to ji_count - 1 do
      if !closed && subset land (1 lsl upper) <> 0
      then
        for lower = 0 to ji_count - 1 do
          if leq.(lower).(upper) && subset land (1 lsl lower) = 0
          then closed := false
        done
    done;
    if !closed then ideals := subset :: !ideals
  done;
  List.rev !ideals
  |> List.sort (fun a b ->
    match Int.compare (popcount a) (popcount b) with
    | 0 -> Int.compare a b
    | n -> n)
  |> Array.of_list

let cover_edges ideals =
  let n = Array.length ideals in
  let subset x y = x land lnot y = 0 in
  let edges = ref [] in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      if i <> j && subset ideals.(i) ideals.(j)
      then (
        let covered = ref true in
        for k = 0 to n - 1 do
          if k <> i
             && k <> j
             && subset ideals.(i) ideals.(k)
             && subset ideals.(k) ideals.(j)
             && ideals.(i) <> ideals.(k)
             && ideals.(k) <> ideals.(j)
          then covered := false
        done;
        if !covered then edges := (i, j) :: !edges)
    done
  done;
  List.rev !edges

let direction_symbol lt = if lt then "<" else ">"

let emit_base_from_poset ~name ~lt leq =
  let ideals = ideals_of_poset leq in
  let element_names =
    Array.init (Array.length ideals) (fun i -> Printf.sprintf "E%d" i)
  in
  let edge_lines =
    match cover_edges ideals with
    | [] -> [ "  " ^ element_names.(0) ]
    | edges ->
      List.map
        (fun (a, b) ->
          let left, right = if lt then a, b else b, a in
          Printf.sprintf
            "  %s %s %s"
            element_names.(left)
            (direction_symbol lt)
            element_names.(right))
        edges
  in
  String.concat
    "\n"
    [ name ^ " = [";
      String.concat ";\n" edge_lines;
      "]"
    ]

let choose lst =
  List.nth lst (Random.State.int rng (List.length lst))

let emit_random_product ~name ~available =
  let field_count = 2 + Random.State.int rng 2 in
  let lines =
    List.init field_count (fun i ->
      let lattice_name = choose available in
      let opposite = if Random.State.bool rng then " ^op" else "" in
      Printf.sprintf
        "  f%d : %s%s"
        (i + 1)
        lattice_name
        opposite)
  in
  String.concat
    "\n"
    [ name ^ " = {";
      String.concat ";\n" lines;
      "}"
    ]

let emit_chain ~name size =
  let elements =
    List.init size (fun i -> Printf.sprintf "E%d" i)
  in
  Printf.sprintf "%s = [\n  %s\n]" name (String.concat " < " elements)

let increasing_injection small_size big_size =
  let rec choose count lower_bound acc =
    if count = 0
    then List.rev acc
    else (
      let remaining = count - 1 in
      let max_start = big_size - remaining - 1 in
      let index =
        lower_bound + Random.State.int rng (max_start - lower_bound + 1)
      in
      choose remaining (index + 1) (index :: acc))
  in
  choose small_size 0 []

let check_increasing = function
  | [] | [ _ ] -> true
  | x :: xs ->
    let rec loop prev = function
      | [] -> true
      | y :: ys -> prev < y && loop y ys
    in
    loop x xs

let emit_chain_embedding_case index =
  let small_size = 2 + Random.State.int rng 3 in
  let big_size = small_size + Random.State.int rng 3 in
  let mapping = increasing_injection small_size big_size in
  if not (check_increasing mapping)
  then Test_support.failf "invalid synthetic embedding";
  let small_name = Printf.sprintf "Small%d" index in
  let big_name = Printf.sprintf "Big%d" index in
  let lines =
    List.mapi
      (fun i target ->
        Printf.sprintf "  E%d -> E%d;" i target)
      mapping
  in
  String.concat
    "\n\n"
    [ emit_chain ~name:small_name small_size;
      emit_chain ~name:big_name big_size;
      String.concat
        "\n"
        [ Printf.sprintf "%s <= %s via {" small_name big_name;
          String.concat "\n" lines;
          "}"
        ]
    ]

let emit_base_product_case index =
  let base_count = 1 + Random.State.int rng 3 in
  let base_names = ref [] in
  let decls = ref [] in
  for i = 0 to base_count - 1 do
    let name = Printf.sprintf "Base%d_%d" index i in
    let ji_count = 1 + Random.State.int rng 3 in
    let source =
      emit_base_from_poset
        ~name
        ~lt:(Random.State.bool rng)
        (make_random_poset ji_count)
    in
    decls := !decls @ [ source ];
    base_names := !base_names @ [ name ]
  done;
  let product_count =
    if Random.State.bool rng then 1 else 0
  in
  for i = 0 to product_count - 1 do
    let name = Printf.sprintf "Prod%d_%d" index i in
    let source = emit_random_product ~name ~available:!base_names in
    decls := !decls @ [ source ]
  done;
  String.concat "\n\n" !decls

let check_source kind index source =
  try
    ignore
      (Generate.render_string
         ~root_module:"Generated"
         ~input_name:(kind ^ ".lattice")
         source);
    Test_support.run_generated_case
      ~name:(Printf.sprintf "%s-%03d" kind index)
      ~source
  with
  | exn -> fail_case kind index source exn

let run () =
  for index = 1 to base_product_case_count do
    check_source "random-base-product" index (emit_base_product_case index)
  done;
  for index = 1 to chain_embedding_case_count do
    check_source "random-chain-embedding" index (emit_chain_embedding_case index)
  done
