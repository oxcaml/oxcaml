[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]

let fatal = Misc.fatal_errorf

(* CR-someday xclerc for xclerc: consider switching to DFS if this naive
   implementation proves to be too slow, or allocates too much. *)
let rec has_cycle : Label.Set.t -> Label.Set.t Label.Tbl.t -> bool =
 fun not_sorted predecessors ->
  match Label.Set.is_empty not_sorted with
  | true ->
    (* all blocks are sorted, hence no cycles *)
    false
  | false -> (
    let without_preds =
      Label.Set.filter
        (fun label ->
          match Label.Tbl.find_opt predecessors label with
          | None ->
            fatal "no information about predecessors for %a" Label.format label
          | Some preds -> Label.Set.is_empty preds)
        not_sorted
    in
    match Label.Set.is_empty without_preds with
    | true ->
      (* no blocks for the next "layer", hence cycle *)
      true
    | false ->
      let not_sorted = Label.Set.diff not_sorted without_preds in
      Label.Tbl.filter_map_inplace
        (fun _label preds -> Some (Label.Set.diff preds without_preds))
        predecessors;
      has_cycle not_sorted predecessors)

(** A way to check whether a CFG is reducible is to:
    + compute the dominators;
    + identify the back edges (i.e. edges such that the destination dominates
      the source);
    + create a copy of the CFG without the back edges;
    + if that copy has a cycle, then the original CFG is irreducible, otherwise
      it is reducible.

    Since a [Cfg_with_infos.t] value already has the dominator information, we
    simply implement cycle detection with a tweaked implementation which ignores
    back edges.

    The current/naive implementation of cycle detection is a variant of Kahn's
    algorithm:
    {ul
     {- we start with a set of blocks/labels to sort equal to the set of all
        blocks;
     }
     {- we compute a table giving for each block the set of its predecessors
        (ignoring back edges);
     }
     {- we look for cycles by:
        - computing the set of blocks with no predecessors;
        - remove these blocks from the table of predecessors;
        - until all blocks have been sorted (in which case there is no cycle),
          or we have no blocks with no predecessors (in which case there is a
          cycle).
     }
    } *)
let is_cfg_with_infos_reducible cfg_with_infos =
  let cfg = Cfg_with_infos.cfg cfg_with_infos in
  let doms = Cfg_with_infos.dominators cfg_with_infos in
  let not_sorted = ref Label.Set.empty in
  let predecessors = Label.Tbl.create (Label.Tbl.length cfg.blocks) in
  Cfg.iter_blocks cfg ~f:(fun label _block ->
      not_sorted := Label.Set.add label !not_sorted;
      Label.Tbl.replace predecessors label Label.Set.empty);
  Cfg.iter_blocks cfg ~f:(fun predecessor_label predecessor_block ->
      let successor_labels =
        Cfg.successor_labels predecessor_block ~normal:true ~exn:true
      in
      Label.Set.iter
        (fun successor_label ->
          match
            Cfg_dominators.is_dominating doms successor_label predecessor_label
          with
          | true ->
            (* destination dominates source: this is a back edge, which is
               ignored *)
            ()
          | false ->
            (* destination does not dominate source: we register the
               predecessor *)
            let preds = Label.Tbl.find predecessors successor_label in
            Label.Tbl.replace predecessors successor_label
              (Label.Set.add predecessor_label preds))
        successor_labels);
  not (has_cycle !not_sorted predecessors)
