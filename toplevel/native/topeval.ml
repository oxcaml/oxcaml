(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The interactive toplevel loop *)

open Format
open Misc
open Parsetree
open Types
open Typedtree
open Outcometree
open Topcommon

let implementation_label = "native toplevel"

let global_symbol comp_unit =
  let sym =
    Symbol.for_compilation_unit comp_unit
    |> Symbol.linkage_name
    |> Linkage_name.to_string
  in
  match Tophooks.lookup sym with
  | None ->
    fatal_error ("Toploop.global_symbol " ^
      (Compilation_unit.full_path_as_string comp_unit))
  | Some obj -> obj

let remembered = ref Ident.empty

let remember phrase_name signature =
  let exported = List.filter Includemod.is_runtime_component signature in
  List.iteri (fun i sg ->
      match sg with
      | Sig_value  (id, _, _)
      | Sig_module (id, _, _, _, _)
      | Sig_typext (id, _, _, _)
      | Sig_class  (id, _, _, _) ->
        remembered := Ident.add id (phrase_name, i) !remembered
      | _ -> ())
    exported

let toplevel_value id =
  try Ident.find_same id !remembered
  with _ -> Misc.fatal_error @@ "Unknown ident: " ^ Ident.unique_name id

let close_phrase lam =
  let open Lambda in
  Ident.Set.fold (fun id l ->
    let glb, pos = toplevel_value id in
    let glob =
      Lprim (Pfield (pos, Pointer, Reads_agree),
             [Lprim (Pgetglobal glb, [], Loc_unknown)],
             Loc_unknown)
    in
    Llet(Strict, Lambda.layout_module_field, id, glob, l)
  ) (free_variables lam) lam

let toplevel_value id =
  let glob, pos =
    if Config.flambda then toplevel_value id else Translmod.nat_toplevel_name id
  in
  (Obj.magic (global_symbol glob)).(pos)

(* Return the value referred to by a path *)

module EvalBase = struct

  let eval_compilation_unit cu =
    try global_symbol cu
    with _ ->
      raise (Undefined_global (cu |> Compilation_unit.full_path_as_string))

  let eval_ident id =
    try toplevel_value id
    with _ ->
      raise (Undefined_global (Ident.name id))

end

include Topcommon.MakeEvalPrinter(EvalBase)

(* Load in-core and execute a lambda term *)

let may_trace = ref false (* Global lock on tracing *)

let load_lambda ppf ~compilation_unit ~required_globals phrase_name lam size =
  if !Clflags.dump_debug_uid_tables then Type_shape.print_debug_uid_tables ppf;
  if !Clflags.dump_rawlambda then fprintf ppf "%a@." Printlambda.lambda lam;
  let slam = Simplif.simplify_lambda lam in
  if !Clflags.dump_lambda then fprintf ppf "%a@." Printlambda.lambda slam;

  let program =
    { Lambda.
      code = slam;
      main_module_block_size = size;
      arg_block_field = None;
      compilation_unit;
      required_globals;
    }
  in
  Tophooks.load ppf phrase_name program

(* Print the outcome of an evaluation *)

let pr_item =
  Printtyp.print_items
    (fun env -> function
      | Sig_value(id, {val_kind = Val_reg; val_type}, _) ->
          Some (outval_of_value env (toplevel_value id) val_type)
      | _ -> None
    )

(* Execute a toplevel phrase *)

let phrase_seqid = ref 0

let name_expression ~loc ~attrs sort exp =
  let name = "_$" in
  let id = Ident.create_local name in
  let vd =
    { val_type = exp.exp_type;
      val_kind = Val_reg;
      val_loc = loc;
      val_attributes = attrs;
      val_uid = Uid.internal_not_actually_unique;
      val_zero_alloc = Default_check }
   in
   let sg = [Sig_value(id, vd, Exported)] in
   let pat =
     { pat_desc = Tpat_var(id, mknoloc name, vd.val_uid,
        Mode.Value.disallow_right Mode.Value.legacy);
       pat_loc = loc;
       pat_extra = [];
       pat_type = exp.exp_type;
       pat_env = exp.exp_env;
       pat_attributes = []; }
   in
   let vb =
     { vb_pat = pat;
       vb_expr = exp;
       vb_rec_kind = Dynamic;
       vb_sort = sort;
       vb_attributes = attrs;
       vb_loc = loc; }
   in
   let item =
     { str_desc = Tstr_value(Nonrecursive, [vb]);
       str_loc = loc;
       str_env = exp.exp_env; }
   in
   let final_env = Env.add_value id vd exp.exp_env in
   let str =
     { str_items = [item];
       str_type = sg;
       str_final_env = final_env }
   in
   str, sg

let execute_phrase print_outcome ppf phr =
  match phr with
  | Ptop_def sstr ->
      let oldenv = !toplevel_env in
      let oldsig = !toplevel_sig in
      incr phrase_seqid;
      let phrase_name = "TOP" ^ string_of_int !phrase_seqid in
      let phrase_comp_unit =
        Compilation_unit.create Compilation_unit.Prefix.empty
          (Compilation_unit.Name.of_string phrase_name)
      in
      Compilenv.reset phrase_comp_unit;
      Typecore.reset_delayed_checks ();
      let (str, sg, names, shape, newenv) =
        Typemod.type_toplevel_phrase oldenv oldsig sstr
      in
      if !Clflags.dump_typedtree then Printtyped.implementation ppf str;
      let sg' = Typemod.Signature_names.simplify newenv names sg in
      Includemod.check_implementation oldenv ~modes:(Legacy None) sg sg';
      Typecore.force_delayed_checks ();
      let shape = Shape_reduce.local_reduce Env.empty shape in
      if !Clflags.dump_shape then Shape.print ppf shape;
      (* `let _ = <expression>` or even just `<expression>` require special
         handling in toplevels, or nothing is displayed. In bytecode, the
         lambda for <expression> is directly executed and the result _is_ the
         value. In native, the lambda for <expression> is compiled and loaded
         from a DLL, and the result of loading that DLL is _not_ the value
         itself. In native, <expression> must therefore be named so that it can
         be looked up after the DLL has been dlopen'd.

         The expression is "named" after typing in order to ensure that both
         bytecode and native toplevels always type-check _exactly_ the same
         expression. Adding the binding at the parsetree level (before typing)
         can create observable differences (e.g. in type variable names, see
         tool-toplevel/topeval.ml in the testsuite) *)
      let str, sg', rewritten =
         match find_eval_phrase str with
         | Some (e, sort, attrs, loc) ->
             let str, sg' = name_expression ~loc ~attrs sort e in
             str, sg', true
         | None -> str, sg', false
      in
      let compilation_unit, res, required_globals, size =
        if Config.flambda then
          let { Lambda.compilation_unit; main_module_block_size = size;
                required_globals; code = res } =
            Translmod.transl_implementation phrase_comp_unit
              (str, Tcoerce_none, None)
              ~style:Plain_block
          in
          remember compilation_unit sg';
          compilation_unit, close_phrase res, required_globals, size
        else
          let size, res = Translmod.transl_store_phrases phrase_comp_unit str in
          phrase_comp_unit, res, Compilation_unit.Set.empty, size
      in
      Warnings.check_fatal ();
      begin try
        toplevel_env := newenv;
        toplevel_sig := List.rev_append sg' oldsig;
        let res =
          load_lambda ppf ~required_globals ~compilation_unit phrase_name res size
        in
        let out_phr =
          match res with
          | Result _ ->
              if Config.flambda then
                (* CR-someday trefis: *)
                Env.register_import_as_opaque
                  (Compilation_unit.name compilation_unit)
              else
                Compilenv.record_global_approx_toplevel ();
              if print_outcome then
                Printtyp.wrap_printing_env ~error:false oldenv (fun () ->
                match str.str_items with
                | [] -> Ophr_signature []
                | _ ->
                    if rewritten then
                      match sg' with
                      | [ Sig_value (id, vd, _) ] ->
                          let outv =
                            outval_of_value newenv (toplevel_value id)
                              vd.val_type
                          in
                          let ty = Printtyp.tree_of_type_scheme vd.val_type in
                          Ophr_eval (outv, ty)
                      | _ -> assert false
                    else
                      Ophr_signature (pr_item oldenv sg'))
              else Ophr_signature []
          | Exception exn ->
              toplevel_env := oldenv;
              toplevel_sig := oldsig;
              if exn = Out_of_memory then Gc.full_major();
              let outv =
                outval_of_value !toplevel_env (Obj.repr exn) Predef.type_exn
              in
              Ophr_exception (exn, outv)
        in
        begin match out_phr with
        | Ophr_signature [] -> ()
        | _ ->
            Location.separate_new_message ppf;
            !print_out_phrase ppf out_phr;
        end;
        begin match out_phr with
        | Ophr_eval (_, _) | Ophr_signature _ -> true
        | Ophr_exception _ -> false
        end
      with x ->
        toplevel_env := oldenv; toplevel_sig := oldsig; raise x
      end
  | Ptop_dir {pdir_name = {Location.txt = dir_name}; pdir_arg } ->
      try_run_directive ppf dir_name pdir_arg


(* API compat *)

let getvalue _ = assert false
let setvalue _ _ = assert false

(* Loading files *)

(* Load in-core a .cmxs file *)

let load_file _ (* fixme *) ppf name0 =
  let name =
    try Some (Load_path.find name0)
    with Not_found -> None
  in
  match name with
  | None -> fprintf ppf "File not found: %s@." name0; false
  | Some name ->
    let fn,tmp =
      if Filename.check_suffix name ".cmx" || Filename.check_suffix name ".cmxa"
      then
        let cmxs = Filename.temp_file "caml" ".cmxs" in
        Asmlink.link_shared ~ppf_dump:ppf [name] cmxs;
        cmxs,true
      else
        name,false
    in
    let success =
      (* The Dynlink interface does not allow us to distinguish between
          a Dynlink.Error exceptions raised in the loaded modules
          or a genuine error during dynlink... *)
      try Dynlink.loadfile fn; true
      with
      | Dynlink.Error err ->
        fprintf ppf "Error while loading %s: %s.@."
          name (Dynlink.error_message err);
        false
      | exn ->
        print_exception_outcome ppf exn;
        false
    in
    if tmp then (try Sys.remove fn with Sys_error _ -> ());
    success

let init () =
  Compmisc.init_path ();
  Clflags.dlcode := true;
  ()
