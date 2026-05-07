open Flambda2_kinds
open Flambda2_parser
open Flambda2_term_basics
open Flambda2_terms

let parse filename text =
  let channel = open_out filename in
  Misc.try_finally
    ~always:(fun () -> close_out channel)
    (fun () -> output_string channel text);
  match Parse_flambda.parse filename with
  | Ok unit -> unit
  | Error (Parsing_error (message, location)) ->
    Misc.fatal_errorf "%a: %s" Location.print_loc location message
  | Error (Lexing_error (error, location)) ->
    Misc.fatal_errorf "%a: %a" Location.print_loc location Flambda_lex.pp_error
      error

let application unit =
  match Flambda.Expr.descr (Flambda_unit.body unit) with
  | Apply apply -> apply
  | Let _ | Let_cont _ | Apply_cont _ | Switch _ | Invalid _ ->
    Misc.fatal_error "Expected an application"

let check ~label ~expected_return unit =
  let apply = application unit in
  (match Apply_expr.call_kind apply with
  | Function { function_call = Indirect_unknown_arity } -> ()
  | Function { function_call = Direct _ | Indirect_known_arity _ }
  | Method _ | C_call _ | Effect _ ->
    Misc.fatal_errorf "%s: expected unknown argument arity" label);
  let expected = expected_return (Flambda_unit.return_continuation unit) in
  if not (Apply_expr.Return.equal expected (Apply_expr.return apply))
  then
    Misc.fatal_errorf "%s: incorrect return constructor or arity:@ %a" label
      Apply_expr.print apply

let source ~annotation ~destination =
  let callee =
    match annotation with
    | None -> "$callee"
    | Some annotation -> Printf.sprintf "($callee : _ -> %s)" annotation
  in
  Printf.sprintf "apply &toplevel %s (0) -> %s * error" callee destination

let test filename =
  let base = parse filename (source ~annotation:None ~destination:"done") in
  let any_value =
    Flambda_arity.create_singletons [Flambda_kind.With_subkind.any_value]
  in
  let value_kind =
    Flambda_kind.With_subkind.create Flambda_kind.value Anything Non_nullable
  in
  let value = Flambda_arity.create_singletons [value_kind] in
  let int64 =
    Flambda_arity.create_singletons [Flambda_kind.With_subkind.naked_int64]
  in
  let product =
    Flambda_arity.create_singletons
      [value_kind; Flambda_kind.With_subkind.naked_int64]
  in
  let returns arity cont : Apply_expr.Return.t = Returns_to { cont; arity } in
  let forwards cont : Apply_expr.Return.t = Tail_forwards_to_caller cont in
  let never arity _cont : Apply_expr.Return.t = Never_returns { arity } in
  let cases =
    [ None, "done", returns any_value;
      Some "val", "done", returns value;
      Some "unknown", "done", forwards;
      Some "bottom", "done", never Bottom;
      Some "int64", "done", returns int64;
      Some "val * int64", "done", returns product;
      Some "unknown", "never", never Unknown;
      Some "bottom", "never", never Bottom;
      Some "int64", "never", never (Result_arity.ok int64) ]
  in
  List.iter
    (fun (annotation, destination, expected_return) ->
      let text = source ~annotation ~destination in
      check ~label:("parsed " ^ text) ~expected_return (parse filename text);
      let return = expected_return (Flambda_unit.return_continuation base) in
      let apply = Apply_expr.with_return (application base) return in
      let unit =
        Flambda_unit.with_body base (Flambda.Expr.create_apply apply)
      in
      check ~label:("constructed " ^ text) ~expected_return unit;
      let printed =
        Misc.Colours.without_colours ~f:(fun () ->
            Format.asprintf "%a" Print_fexpr.flambda_unit
              (Flambda_to_fexpr.conv unit))
      in
      check ~label:("roundtrip " ^ text) ~expected_return
        (parse filename printed))
    cases

let () =
  let filename = Filename.temp_file "fexpr_result_arity" ".fl" in
  Misc.try_finally
    ~always:(fun () -> Sys.remove filename)
    (fun () ->
      Env.set_current_unit (Parse_flambda.make_unit_info ~filename);
      test filename)
