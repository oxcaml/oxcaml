(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Florian Angeletti, projet Cambium, INRIA Paris                        *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Out_type
module Fmt = Format_doc

let namespaced_ident namespace  id =
  Out_name.print (ident_name (Some namespace) id)

module Doc = struct
  let wrap_printing_env = wrap_printing_env

  let longident = Pprintast.Doc.longident

  let ident ppf id = Fmt.pp_print_string ppf
      (Out_name.print (ident_name None id))

let instance_name global =
  (* Construct the stopgap syntax and then shove it in a string with the
     attribute after it. *)
  (* CR lmaurer: This is hacky and it loses the state of the [out_name]s that
     comprise the [out_ident]. Should presumably have a new constructor for
     [out_ident] instead? *)
  let rec string_of_global global =
    (* We can avoid calling [ident_name_simple] here because instance names are
       always global (which is bad - but the syntax is currently bad anyway) *)
    let ({ head; args } : Global_module.Name.t) = global in
    String.concat "" (head :: List.map string_of_arg args)
  and string_of_arg arg =
    let ({ param; value } : Global_module.Name.argument) = arg in
    Printf.sprintf "(%s)(%s)"
      (Global_module.Parameter_name.to_string param) (string_of_global value)
  in
  let printed_name =
    string_of_global global ^ " [@jane.non_erasable.instances]"
  in
  { printed_name }



  let typexp mode ppf ty =
    !Oprint.out_type ppf (tree_of_typexp mode ty)

  let type_expansion k ppf e =
    pp_type_expansion ppf (trees_of_type_expansion k e)

  let type_declaration id ppf decl =
    !Oprint.out_sig_item ppf (tree_of_type_declaration id decl Trec_first)

  let type_expr ppf ty =
    (* [type_expr] is used directly by error message printers,
       we mark eventual loops ourself to avoid any misuse and stack overflow *)
    prepare_for_printing [ty];
    prepared_type_expr ppf ty

  let shared_type_scheme ppf ty =
    add_type_to_preparation ty;
    typexp Type_scheme ppf ty

  let type_scheme ppf ty =
    prepare_for_printing [ty];
    prepared_type_scheme ppf ty

  let path ppf p =
    !Oprint.out_ident ppf (tree_of_path ~disambiguation:false p)

  let () = Env.print_path := path

  let type_path ppf p = !Oprint.out_ident ppf (tree_of_type_path p)

  let value_description id ppf decl =
    !Oprint.out_sig_item ppf (tree_of_value_description id decl)

  let class_type ppf cty =
    reset ();
    prepare_class_type cty;
    !Oprint.out_class_type ppf (tree_of_class_type Type cty)

  let class_declaration id ppf cl =
    !Oprint.out_sig_item ppf (tree_of_class_declaration id cl Trec_first)

  let cltype_declaration id ppf cl =
    !Oprint.out_sig_item ppf (tree_of_cltype_declaration id cl Trec_first)

  let modtype ppf mty = !Oprint.out_module_type ppf (tree_of_modtype mty)
  let modtype_declaration id ppf decl =
    !Oprint.out_sig_item ppf (tree_of_modtype_declaration id decl)

  let constructor ppf c =
    reset_except_conflicts ();
    add_constructor_to_preparation c;
    prepared_constructor ppf c

  let constructor_arguments ppf a =
    let tys = tree_of_constructor_arguments a in
    !Oprint.out_type ppf (Otyp_tuple (List.map (fun t -> None, t) tys))

  let label ppf l =
    prepare_for_printing [l.Types.ld_type];
    !Oprint.out_label ppf (tree_of_label l)

  let extension_constructor id ppf ext =
    !Oprint.out_sig_item ppf (tree_of_extension_constructor id ext Text_first)

  (* Print an extension declaration *)



  let extension_only_constructor id ppf (ext:Types.extension_constructor) =
    reset_except_conflicts ();
    prepare_type_constructor_arguments ext.ext_args;
    Option.iter add_type_to_preparation ext.ext_ret_type;
    let name = Ident.name id in
    let args, ret =
      extension_constructor_args_and_ret_type_subtree
        ext.ext_args
        ext.ext_ret_type
    in
    Fmt.fprintf ppf "@[<hv>%a@]"
      !Oprint.out_constr {
      Outcometree.ocstr_name = name;
      ocstr_args = args;
      ocstr_return_type = ret;
    }

  (* Print a signature body (used by -i when compiling a .ml) *)

  let print_signature ppf tree =
    Fmt.fprintf ppf "@[<v>%a@]" !Oprint.out_signature tree

  let signature ppf sg =
    Fmt.fprintf ppf "%a" print_signature (tree_of_signature sg)

end
open Doc
let string_of_path p = Fmt.asprintf "%a" path p

let strings_of_paths namespace p =
  let trees = List.map (namespaced_tree_of_path namespace) p in
  List.map (Fmt.asprintf "%a" !Oprint.out_ident) trees

let wrap_printing_env = wrap_printing_env
let ident = Fmt.compat ident
let longident = Fmt.compat longident
let path = Fmt.compat path
let type_path = Fmt.compat type_path
let type_expr = Fmt.compat type_expr
let type_scheme = Fmt.compat type_scheme
let shared_type_scheme = Fmt.compat shared_type_scheme

let type_declaration  = Fmt.compat1 type_declaration
let type_expansion = Fmt.compat1 type_expansion
let value_description = Fmt.compat1 value_description
let label = Fmt.compat label
let constructor = Fmt.compat constructor
let constructor_arguments = Fmt.compat constructor_arguments
let extension_constructor = Fmt.compat1 extension_constructor
let extension_only_constructor = Fmt.compat1 extension_only_constructor

let modtype = Fmt.compat modtype
let modtype_declaration = Fmt.compat1 modtype_declaration
let signature = Fmt.compat signature

let class_declaration = Fmt.compat1 class_declaration
let class_type = Fmt.compat class_type
let cltype_declaration = Fmt.compat1 cltype_declaration

(* Print a signature body (used by -i when compiling a .ml) *)
let printed_signature sourcefile ppf sg =
  (* we are tracking any collision event for warning 63 *)
  Ident_conflicts.reset ();
  let t = tree_of_signature sg in
<<<<<<< oxcaml
  if Warnings.(is_active @@ Erroneous_printed_signature "")
  && Conflicts.exists ()
  then begin
    let conflicts = Format_doc.asprintf "%t" Conflicts.print_explanations in
    Location.prerr_warning (Location.in_file sourcefile)
      (Warnings.Erroneous_printed_signature conflicts);
    Warnings.check_fatal ()
  end;
  compat print_signature ppf t

(** Compatibility module for Format printers *)
module Compat = struct
  let longident = Fmt.compat longident
  let path = Fmt.compat path
  let type_expr = Fmt.compat type_expr
  let shared_type_scheme = Fmt.compat shared_type_scheme
  let signature = Fmt.compat signature
  let class_type = Fmt.compat class_type
  let modtype = Fmt.compat modtype
  let string_of_label (lbl : Asttypes.arg_label) =
    let lbl : Types.arg_label = match lbl with
      | Nolabel -> Nolabel
      | Labelled s -> Labelled s
      | Optional s -> Optional s
    in
    string_of_label lbl
end
||||||| upstream-base
  if Warnings.(is_active @@ Erroneous_printed_signature "")
  && Conflicts.exists ()
  then begin
    let conflicts = Format.asprintf "%t" Conflicts.print_explanations in
    Location.prerr_warning (Location.in_file sourcefile)
      (Warnings.Erroneous_printed_signature conflicts);
    Warnings.check_fatal ()
  end;
  fprintf ppf "%a" print_signature t
=======
  if Warnings.(is_active @@ Erroneous_printed_signature "") then
    begin match Ident_conflicts.err_msg () with
    | None -> ()
    | Some msg ->
        let conflicts = Fmt.asprintf "%a" Fmt.pp_doc msg in
        Location.prerr_warning (Location.in_file sourcefile)
          (Warnings.Erroneous_printed_signature conflicts);
        Warnings.check_fatal ()
    end;
  Fmt.compat print_signature ppf t
>>>>>>> upstream-incoming
