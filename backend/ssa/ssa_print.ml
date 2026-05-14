open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-40-41-42"]

(** Block- and graph-level pretty-printers for {!Ssa.Finished_graph}. The
    instruction-, terminator- and block-id printers live in {!Ssa} itself, so
    they are shared between the [Graph_builder] and [Finished_graph] views. *)

module Make (S : Ssa.Finished_graph) = struct
  let print_typed_params ppf (blk : S.Block.t) =
    Array.iteri
      (fun i (p : S.Block.param) ->
        if i > 0 then Format.fprintf ppf ", ";
        S.Block.print_param ppf blk i;
        Format.fprintf ppf " : %a" Printcmm.machtype_component p.typ)
      blk.params

  let print_block_header ppf (blk : S.Block.t) =
    let name = if blk.is_function_start then "FUNCTION_START" else "BLOCK" in
    Format.fprintf ppf "%a: %s(%a)" S.Block.print_id blk name print_typed_params
      blk;
    Format.fprintf ppf " [idom=%a depth=%d]" S.Block.print_id
      blk.dominator_info.dominator blk.dominator_info.depth;
    match S.Block.predecessors blk with
    | [] -> ()
    | preds ->
      Format.fprintf ppf " <- %a"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
           S.Block.print_id)
        preds

  let print_block ppf (blk : S.Block.t) =
    Format.fprintf ppf "%a@." print_block_header blk;
    Array.iter
      (fun bi -> Format.fprintf ppf "  %a@." S.Instruction.print bi)
      blk.body;
    Format.fprintf ppf "  %a" S.Terminator.print blk.terminator;
    (match S.Block.trap_successor blk with
    | None -> ()
    | Some h -> Format.fprintf ppf " trap_successor=%a" S.Block.print_id h);
    Format.fprintf ppf "@.@."

  let print ppf =
    Format.fprintf ppf "ssa %s(%a)@." S.function_info.name Printcmm.machtype
      S.function_info.args;
    Format.fprintf ppf "  entry = %a@.@." S.Block.print_id S.entry;
    List.iter (print_block ppf) S.blocks;
    Format.fprintf ppf "@."
end

let print ppf (m : (module Ssa.Finished_graph)) =
  let module S = (val m : Ssa.Finished_graph) in
  let module P = Make (S) in
  P.print ppf
