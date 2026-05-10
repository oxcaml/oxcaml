open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-40-41-42"]

(** Block- and graph-level pretty-printers for {!Ssa.Finished_graph}. The
    instruction-, terminator- and block-id printers live in {!Ssa} itself, so
    they are shared between the [Graph_builder] and [Finished_graph] views. *)

module Make (S : Ssa.Finished_graph) = struct
  let print_block_id = S.print_block_id

  let print_instruction = S.print_instruction

  let print_terminator = S.print_terminator

  let print_block_param ppf (block : S.Block.t) index =
    match block.params.(index).name with
    | None -> Format.fprintf ppf "%a.%d" print_block_id block index
    | Some n -> Format.fprintf ppf "%s/%a.%d" n print_block_id block index

  let print_typed_params ppf (blk : S.Block.t) =
    Array.iteri
      (fun i (p : S.Block.param) ->
        if i > 0 then Format.fprintf ppf ", ";
        print_block_param ppf blk i;
        Format.fprintf ppf " : %a" Printcmm.machtype_component p.typ)
      blk.params

  let print_block_header ppf (blk : S.Block.t) =
    let name = if blk.is_function_start then "FUNCTION_START" else "BLOCK" in
    Format.fprintf ppf "%a: %s(%a)" print_block_id blk name print_typed_params
      blk;
    Format.fprintf ppf " [idom=%a depth=%d]" print_block_id
      blk.dominator_info.dominator blk.dominator_info.depth;
    match S.predecessors blk |> S.Block.Set.elements with
    | [] -> ()
    | preds ->
      Format.fprintf ppf " <- %a"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
           print_block_id)
        preds

  let print_block ppf (blk : S.Block.t) =
    Format.fprintf ppf "%a@." print_block_header blk;
    Array.iter
      (fun bi -> Format.fprintf ppf "  %a@." print_instruction bi)
      blk.body;
    Format.fprintf ppf "  %a" print_terminator blk.terminator;
    (match S.trap_successor blk with
    | None -> ()
    | Some h -> Format.fprintf ppf " trap_successor=%a" print_block_id h);
    Format.fprintf ppf "@.@."

  let print ppf =
    Format.fprintf ppf "ssa %s(%a)@." S.function_info.fun_name Printcmm.machtype
      S.function_info.fun_args;
    Format.fprintf ppf "  entry = %a@.@." print_block_id S.entry;
    List.iter (print_block ppf) S.blocks;
    Format.fprintf ppf "@."
end

let print ppf (m : (module Ssa.Finished_graph)) =
  let module S = (val m : Ssa.Finished_graph) in
  let module P = Make (S) in
  P.print ppf
