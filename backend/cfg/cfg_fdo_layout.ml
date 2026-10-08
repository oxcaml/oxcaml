[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare
module DLL = Doubly_linked_list

let build_layout counts cfg_with_layout =
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  (* Original positions, for deterministic tie-breaking. *)
  let position = Label.Tbl.create (Label.Tbl.length cfg.blocks) in
  DLL.iter (Cfg_with_layout.layout cfg_with_layout) ~f:(fun label ->
      Label.Tbl.replace position label (Label.Tbl.length position));
  let position label = Label.Tbl.find position label in
  (* Block sizes are estimated at 4 bytes per instruction. *)
  let blocks =
    DLL.to_list (Cfg_with_layout.layout cfg_with_layout)
    |> List.map (fun label ->
        let block = Cfg.get_block_exn cfg label in
        ( label,
          4 * (DLL.length block.body + 1),
          Cfg_fdo_counts.block_count counts label ))
    |> Array.of_list
  in
  let edges =
    Cfg.fold_blocks cfg ~init:[] ~f:(fun src _ acc ->
        List.fold_left
          (fun acc (dst, weight) -> (src, dst, weight) :: acc)
          acc
          (Cfg_fdo_counts.successor_edge_weights counts src))
    |> List.sort (fun (src1, dst1, _) (src2, dst2, _) ->
        let c = Int.compare (position src1) (position src2) in
        if c <> 0 then c else Int.compare (position dst1) (position dst2))
  in
  Cfg_fdo_ext_tsp.layout ~blocks ~edges

let reorder_blocks counts cfg_with_layout =
  if Cfg_fdo_counts.is_hot counts
  then
    Cfg_with_layout.set_layout cfg_with_layout
      (DLL.of_list (build_layout counts cfg_with_layout))
