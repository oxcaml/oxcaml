[@@@ocaml.warning "+a-40-41-42"]

module Graph = struct
  type t =
    { entry : Label.t;
      nodes : Label.t list;
      edges : (Label.t * Label.t) list
    }

  let create (cfg : Cfg.t) =
    (* CR-someday hwasilewski for xclerc: If this were built from a
       [Cfg_with_layout.t], [nodes] could be obtained from its layout, perhaps
       modulo mutability. *)
    let nodes = cfg.blocks |> Label.Tbl.to_seq_keys |> List.of_seq in
    let edges =
      Cfg.fold_blocks cfg ~init:[] ~f:(fun source block edges ->
          Label.Set.fold
            (fun target edges -> (source, target) :: edges)
            (Cfg.successor_labels ~normal:true ~exn:true block)
            edges)
    in
    { entry = cfg.entry_label; nodes; edges }
end
