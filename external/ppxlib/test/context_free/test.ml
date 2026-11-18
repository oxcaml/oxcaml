open Ppxlib

(* [Context_free.map_top_down] is public, and has the methods of any AST traversal. The
   driver only calls [structure] and [signature_items], which handle each of their items
   themselves, so ppxlib never calls [structure_item] or [signature_item]: this is a unit
   test of those two. A whole-file pass would call them for the items it generates, as the
   driver has applied the context-free rules by the time it runs. *)

let expand =
  let number =
    Extension.declare "number" Extension.Context.Core_type
      Ast_pattern.(pstr nil)
      (fun ~loc ~path:_ -> [%type: int])
  in
  new Context_free.map_top_down [ Context_free.Rule.extension number ]

let ctxt =
  Expansion_context.Base.top_level ~tool_name:"test" ~file_path:"test.ml"
    ~input_name:"test.ml"

let loc = Location.none

let () =
  let item, errors = expand#structure_item ctxt [%stri type t = [%number]] in
  assert (errors = []);
  Format.printf "%a@." Pprintast.structure_item item

let () =
  let item, errors = expand#signature_item ctxt [%sigi: val x : [%number]] in
  assert (errors = []);
  Format.printf "%a@." Pprintast.signature_item item
