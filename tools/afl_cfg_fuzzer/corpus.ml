[@@@ocaml.warning "+a-40-41-42"]

let write directory =
  let write name ~node_count ~normal_edges ~exn_edges =
    let bytes = Buffer.create 32 in
    Buffer.add_uint16_le bytes (node_count - 1);
    let add_edges edges ~exceptional =
      List.iter
        (fun (source, target) ->
          let source = if exceptional then source lor 0x8000 else source in
          Buffer.add_uint16_le bytes source;
          Buffer.add_uint16_le bytes target)
        edges
    in
    add_edges normal_edges ~exceptional:false;
    add_edges exn_edges ~exceptional:true;
    Out_channel.with_open_bin
      (Filename.concat directory (name ^ ".bin"))
      (fun channel -> Buffer.output_buffer channel bytes)
  in
  (* Two branches reconverge at a common successor. *)
  write "diamond" ~node_count:4
    ~normal_edges:[0, 1; 0, 2; 1, 3; 2, 3]
    ~exn_edges:[];
  (* A single-entry loop with a back edge from node 2 to node 1. *)
  write "loop" ~node_count:3 ~normal_edges:[0, 1; 1, 2; 2, 1] ~exn_edges:[];
  (* An irreducible loop with entries at nodes 1 and 2. *)
  write "irreducible" ~node_count:4
    ~normal_edges:[0, 1; 0, 2; 1, 3; 2, 3; 3, 1; 3, 2]
    ~exn_edges:[];
  (* A block with distinct normal and exceptional successors. *)
  write "exn" ~node_count:3 ~normal_edges:[0, 1] ~exn_edges:[0, 2]
