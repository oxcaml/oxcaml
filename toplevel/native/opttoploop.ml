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
open Config
open Misc
open Parsetree
open Types
open Typedtree
open Outcometree
open Ast_helper

module Genprintval = Genprintval_native

(* Eta-expansion gives [create_process] and [waitpid] the unannotated types
   required by [Compiler_owee.Unix_intf.S]. *)
module Unix_for_owee = struct
  include Unix

  let create_process prog args stdin stdout stderr =
    Unix.create_process prog args stdin stdout stderr

  let waitpid flags pid = Unix.waitpid flags pid
end

type input =
  | Stdin
  | File of string
  | String of string

let filename_of_input = function
  | File name -> name
  | Stdin | String _ -> ""

type res = Ok of Obj.t | Err of string
type evaluation_outcome = Result of Obj.t | Exception of exn

let _dummy = (Ok (Obj.magic 0), Err "")

module Jit = struct
  type t =
    {
      load : phrase_name:string -> Format.formatter -> Lambda.program
        -> evaluation_outcome;
      lookup_symbol : string -> Obj.t option;
    }
end

let jit = ref None

let register_jit j = jit := Some j

external ndl_run_toplevel: string -> string -> res
  = "caml_natdynlink_run_toplevel"

external max_domain_count : unit -> int
  = "caml_sys_exit" "caml_max_domain_count"
  [@@noalloc]

external code_is_fully_polled : unit -> bool
  = "caml_sys_exit" "caml_code_is_fully_polled"
  [@@noalloc]

(* The native toplevel and eval compile phrases in-process with the
   current flags, so a phrase's polledness is [!Clflags.poll_insertion]
   at compile time (there is no cmx record to consult).  Loading a
   non-polled phrase into a process whose domain cap exceeds 1 would
   falsify the fully-polled property the cap was derived from: this is
   the in-process analogue of natdynlink's [Library_not_fully_polled]
   rejection.  The cap above 1 implies the host itself is fully polled
   (or ran with an explicit OCAMLRUNPARAM d= override), so under default
   flags this only fires when the phrase flags diverge from the host's
   build, which the toplevel's host-variant pin already makes rare.

   No friendlier alternative (e.g. silently enabling poll insertion for
   the phrase) is needed, because a legitimately multi-domain host can
   never reach this failure under default phrase flags: an eval-using
   program links the DEFAULT variant's compiler-libs (the linker's
   [set_load_path_for_eval] rejects eval under any other codegen variant
   and resolves the eval support libraries against the default library
   root).  So when the install's default variant lacks poll insertion,
   those compiler-libs units are unpolled and the host is never fully
   polled -- its cap defaults to 1 and the check passes vacuously; and
   when the default variant includes poll insertion, phrases compile
   with poll insertion on by default and the check passes positively.
   The failure therefore fires only on explicit user divergence (an
   OCAMLRUNPARAM d= override of an unpolled host's cap, or disabling
   poll insertion for phrases on a poll-default install), where failing
   with this message is the right answer. *)
let check_poll_insertion_for_in_process_load () =
  (* As in [Compilenv.build_unit_info], the LLVM backend discards [Poll]
     instructions, so code it compiles is nonpolled even when
     [Clflags.poll_insertion] is set. *)
  let phrase_polled =
    !Clflags.poll_insertion && not !Clflags.llvm_backend
  in
  if (not phrase_polled) && code_is_fully_polled () && max_domain_count () > 1
  then
    failwith
      (if !Clflags.poll_insertion
       then
         "in-process loading of code compiled by the LLVM backend, which \
          inserts no polls, is not permitted in a fully polled program that \
          may run more than one domain; compile the code to load with the \
          native backend"
       else
         "in-process loading of code compiled without poll insertion is not \
          permitted in a fully polled program that may run more than one \
          domain; compile the code to load with poll insertion, which is \
          the default in such a program, or run the program with \
          OCAMLRUNPARAM=d=1 to permit the load")

(* The JIT takes the code from the native backend as it emits it. The LLVM
   backend writes an object file instead, which no setting of the program
   makes loadable that way: a loader that goes through the JIT refuses it
   before it checks poll insertion, whose remedies would not help. (The
   native toplevel without the JIT loads the object files of either
   backend.) *)
let check_backend_for_jit_load () =
  if !Clflags.llvm_backend
  then
    failwith
      "the JIT cannot load code compiled by the LLVM backend; compile the \
       code to load with the native backend"

(* The codegen variant that this executable compiles phrases for by
   default.  Its whole-program axes are those that this library was built
   for: the per-context constants baked in by the toplevel_host_variant
   rule in toplevel/native/dune.  They are not always those of
   [Codegen_variant.default]: a copy of this library built in a
   codegen-variant context is built for that context's variant.  Poll
   insertion is a property of each unit and has no such constant: phrases
   get it by default when every unit of the program has it, so that
   loading them keeps the program fully polled. *)
let host_variant : Codegen_variant.t =
  { address_sanitizer = Toplevel_host_variant.address_sanitizer;
    frame_pointers = Toplevel_host_variant.frame_pointers;
    poll_insertion = code_is_fully_polled ();
    stack_checks = Toplevel_host_variant.stack_checks
  }

(* Whether code compiled for [variant] can be loaded into this executable:
   the whole-program axes must be the host's.  Poll insertion is checked on
   its own, against the domain cap
   ([check_poll_insertion_for_in_process_load]). *)
let variant_matches_host variant =
  Codegen_variant.equal_fingerprint
    (Codegen_variant.fingerprint variant)
    (Codegen_variant.fingerprint host_variant)

let check_codegen_variant_for_in_process_load () =
  (* The flags themselves, which are what the backend reads: for
     [Codegen_variant.current] a JSIR target hides them. *)
  let current_variant = Codegen_variant.flags_variant () in
  if not (variant_matches_host current_variant) then
    failwith
      (Printf.sprintf
         "this executable was built for codegen variant '%s' and cannot \
          compile and load phrases for variant '%s' into its own image; \
          adjust the variant-selecting flags to match the variant the \
          executable was built for"
         (Codegen_variant.to_string host_variant)
         (Codegen_variant.to_string current_variant));
  (* Phrases are compiled to native code whatever the target is, but the
     middle end prepares them for the target: as it does for JSIR under a
     JSIR target, and not at all under the bytecode one, which is the
     target until a program sets another. *)
  if not (Clflags.target_is_native ()) then
    failwith
      "this executable compiles phrases to native code and loads them into \
       its own image, which it can only do for the native target; set that \
       target (Clflags.set_target) before compiling a phrase"

let default_lookup sym =
  Dynlink.unsafe_get_global_value ~bytecode_or_asm_symbol:sym

let global_symbol comp_unit =
  let lookup =
    match !jit with
      | None -> default_lookup
      | Some {Jit.lookup_symbol; _} -> lookup_symbol
  in
  let linkage_name =
    Symbol.for_compilation_unit comp_unit
    |> Symbol.linkage_name
    |> Linkage_name.to_string
  in
  match lookup linkage_name with
  | None ->
    fatal_error ("Opttoploop.global_symbol " ^
      (Compilation_unit.full_path_as_string comp_unit))
  | Some obj -> obj

let need_symbol sym =
  not (Dynlink.does_symbol_exist ~bytecode_or_asm_symbol:sym)

let dll_run dll entry =
  match (try Result (Obj.magic (ndl_run_toplevel dll entry))
         with exn -> Exception exn)
  with
    | Exception _ as r -> r
    | Result r ->
        match Obj.magic r with
          | Ok x -> Result x
          | Err s -> fatal_error ("Opttoploop.dll_run " ^ s)


type directive_fun =
   | Directive_none of (unit -> unit)
   | Directive_string of (string -> unit)
   | Directive_int of (int -> unit)
   | Directive_ident of (Longident.t -> unit)
   | Directive_bool of (bool -> unit)

type directive_info = {
  section: string;
  doc: string;
}

let remembered = ref Ident.empty

let remember phrase_name signature repr =
  let exported = List.filter Includemod.is_runtime_component signature in
  List.iteri (fun i sg ->
    match sg with
    | Sig_value  (id, _, _)
    | Sig_module (id, _, _, _, _)
    | Sig_typext (id, _, _, _)
    | Sig_class  (id, _, _, _) ->
      remembered := Ident.add id (phrase_name, i, repr) !remembered
    | _ -> ())
    exported

let toplevel_value id =
  try Ident.find_same id !remembered
  with _ -> failwith ("Unknown ident: " ^ Ident.unique_name id)

let phrase_static_data : Slambda.CU_data.t Compilation_unit.Tbl.t =
  Compilation_unit.Tbl.create 16

let cu_static_data cu =
  match Compilation_unit.Tbl.find_opt phrase_static_data cu with
  | Some _ as data -> data
  | None -> Compilenv.get_static_data cu

let close_phrase lam =
  let open Lambda in
  Ident.Set.fold (fun id l ->
    let glb, pos, repr = toplevel_value id in
    let layout = Lambda.layout_of_module_field repr pos in
    let glob =
      Lprim (mod_field pos repr,
             [Lprim (Pgetglobal (glb, Static), [], Loc_unknown)],
             Loc_unknown)
    in
    Llet(Strict, layout, id, Lambda.debug_uid_none, glob, l)
  ) (free_variables lam) lam

(* Return the value referred to by a path *)

(* Like [Obj.field], but handles field reordering in mixed modules.
   Returns [None] if the selected field is not a value, [Some field] otherwise.
   Copied from [Topcommon] *)
let mod_field obj (module_repr : Lambda.module_representation) pos =
  if not (Clflags.target_is_native ()) then
    Some (Obj.field obj pos)
  else
  match module_repr with
  | Module_value_only _ -> Some (Obj.field obj pos)
  | Module_mixed (shape, _) ->
    let shape =
      Mixed_block_shape.of_mixed_block_elements shape
        ~print_locality:(fun ppf () -> Format.fprintf ppf "()")
    in
    let new_pos =
      match Mixed_block_shape.lookup_path_producing_new_indexes shape [pos] with
      | [new_pos] -> Some new_pos
      | _ -> None (* [pos] points to an unboxed product or void *)
    in
    Option.bind new_pos
      (fun new_pos ->
        if new_pos < Mixed_block_shape.value_prefix_len shape
        then Some (Obj.field obj new_pos)
        else None (* [pos] points to an unboxed singleton *))

let rec eval_address = function
  | Env.Aunit (cu, _) ->
      global_symbol cu
  | Env.Alocal id ->
      let glob, pos, repr = toplevel_value id in
      begin match mod_field (global_symbol glob) repr pos with
      | Some field -> field
      | None ->
        Location.raise_errorf
          ~loc:Location.none
          "Opttoploop.eval_address: Can't return a non-value"
      end
  | Env.Adot(a, module_repr, pos) ->
      let module_repr = Lambda.transl_module_representation module_repr in
      begin match mod_field (eval_address a) module_repr pos with
      | Some field -> field
      | None ->
        Location.raise_errorf
          ~loc:Location.none
          "Opttoploop.eval_address: Can't return a non-value"
      end

let eval_path find env path =
  match find ~loc:Location.none path env with
  | addr -> eval_address addr
  | exception Not_found ->
      fatal_error ("Cannot find address for: " ^ (Path.name path))

let eval_module_path env path =
  eval_path Env.find_module_address env path

let eval_value_path env path =
  eval_path Env.find_value_address env path

let eval_extension_path env path =
  eval_path Env.find_constructor_address env path

let eval_class_path env path =
  eval_path Env.find_class_address env path

(* To print values *)

module EvalPath = struct
  type valu = Obj.t
  exception Error
  let eval_address addr =
    try eval_address addr with _ -> raise Error
  let same_value v1 v2 = (v1 == v2)
end

module Printer = Genprintval.Make(Obj)(EvalPath)

let max_printer_depth = ref 100
let max_printer_steps = ref 300

let print_out_value = Oprint.out_value
let print_out_type = Oprint.out_type
let print_out_class_type = Oprint.out_class_type
let print_out_module_type = Oprint.out_module_type
let print_out_type_extension = Oprint.out_type_extension
let print_out_sig_item = Oprint.out_sig_item
let print_out_signature = Oprint.out_signature
let print_out_phrase = Oprint.out_phrase

let print_untyped_exception ppf obj =
  !print_out_value ppf (Printer.outval_of_untyped_exception obj)
let outval_of_value env obj lpoly ty =
  Printer.outval_of_value !max_printer_steps !max_printer_depth
    (fun _ _ _ -> None) env obj lpoly ty
let print_value env obj ppf ty =
  !print_out_value ppf (outval_of_value env obj (Lpoly.determined []) ty)

type ('a, 'b) gen_printer = ('a, 'b) Genprintval.gen_printer =
  | Zero of 'b
  | Succ of ('a -> ('a, 'b) gen_printer)

let install_printer = Printer.install_printer
let install_generic_printer = Printer.install_generic_printer
let install_generic_printer' = Printer.install_generic_printer'
let remove_printer = Printer.remove_printer

(* Hooks for parsing functions *)

let parse_toplevel_phrase = ref Parse.toplevel_phrase
let parse_use_file = ref Parse.use_file
let print_location = Location.print_loc
let print_error = Location.print_report
let print_warning = Location.print_warning
let input_name = Location.input_name

let parse_mod_use_file name lb =
  let modname =
    String.capitalize_ascii
      (Filename.remove_extension (Filename.basename name))
  in
  let items =
    List.concat
      (List.map
         (function Ptop_def s -> s | Ptop_dir _ -> [])
         (!parse_use_file lb))
  in
  [ Ptop_def
      [ Str.module_
          (Mb.mk
             (Location.mknoloc (Some modname))
             (Mod.structure items)
          )
       ]
   ]

(* Hook for initialization *)

let toplevel_startup_hook = ref (fun () -> ())

type event = ..
type event +=
  | Startup
  | After_setup

let hooks = ref []

let add_hook f = hooks := f :: !hooks

let () =
  add_hook (function
      | Startup -> !toplevel_startup_hook ()
      | _ -> ())

let run_hooks hook = List.iter (fun f -> f hook) !hooks

(* Load in-core and execute a lambda term *)

let phrase_seqid = ref 0
let phrase_name = ref "TOP"

let use_existing_compilerlibs_state_for_artifacts_flag = ref false

let use_existing_compilerlibs_state_for_artifacts () =
  use_existing_compilerlibs_state_for_artifacts_flag := true

let using_existing_compilerlibs_state_for_artifacts () =
  !use_existing_compilerlibs_state_for_artifacts_flag

let default_load ppf (program : Lambda.program) =
  let dll =
    if !Clflags.keep_asm_file then !phrase_name ^ ext_dll
    else Filename.temp_file ("caml" ^ !phrase_name) ext_dll
  in
  let filename = Filename.chop_extension dll in
  (* TODO: Determine machine_width properly from target configuration *)
  let machine_width = Target_system.Machine_width.Sixty_four in
  let pipeline : Asmgen.pipeline =
    Direct_to_cmm
      (Flambda2.lambda_to_cmm ~machine_width ~keep_symbol_tables:true)
  in
  Asmgen.compile_implementation
    (module Unix_for_owee : Compiler_owee.Unix_intf.S)
    ~toplevel:need_symbol
    ~sourcefile:(Some filename) ~prefixname:filename
    ~pipeline ~ppf_dump:ppf
    program;
  Asmlink.call_linker_shared ~native_toplevel:true [filename ^ ext_obj] dll;
  Sys.remove (filename ^ ext_obj);
  let dll =
    if Filename.is_implicit dll
    then Filename.concat (Sys.getcwd ()) dll
    else dll in
  (* CR-someday lmaurer: The manual prefixing here feels wrong. Probably
     [!phrase_name] should be a [Compilation_unit.t] (from which we can extract
     a linkage name like civilized folk). That will be easier to do once we have
     better types in, say, the [Translmod] API. *)
  let res = dll_run dll ("caml" ^ !phrase_name) in
  (try Sys.remove dll with Sys_error _ -> ());
  (* note: under windows, cannot remove a loaded dll
     (should remember the handles, close them in at_exit, and then remove
     files) *)
  res

let load_tlambda ppf ~compilation_unit ~required_globals tlam repr =
  (* [prepare] pins ocamlnat's startup to [host_variant], but phrase
     loading is also reached by embedders that never call [prepare]
     (expect.opt supplies [initialize_toplevel_env]/[execute_phrase]
     directly), so the admission check must sit on the shared load path
     itself: compiled phrases execute in this executable's image and must
     use the variant it was built for. *)
  check_codegen_variant_for_in_process_load ();
  (* A registered JIT loads the phrase: see [check_backend_for_jit_load]. *)
  if Option.is_some !jit then check_backend_for_jit_load ();
  check_poll_insertion_for_in_process_load ();
  if !Clflags.dump_debug_uid_tables then Type_shape.print_debug_uid_tables ppf;
  if !Clflags.dump_tlambda then fprintf ppf "%a@." Printlambda.lambda tlam;
  let (static_data, rawlam) = Slambda.eval ~cu_static_data tlam in
  Compilation_unit.Tbl.replace phrase_static_data compilation_unit static_data;
  if !Clflags.dump_rawlambda then fprintf ppf "%a@." Printlambda.lambda rawlam;
  let lam =
    Simplif.simplify_lambda rawlam
      ~restrict_to_upstream_dwarf:
        !Clflags.restrict_to_upstream_dwarf
      ~gdwarf_may_alter_codegen:!Dwarf_flags.gdwarf_may_alter_codegen
  in
  if !Clflags.dump_lambda then fprintf ppf "%a@." Printlambda.lambda lam;
  let program =
    { Lambda.
      code = lam;
      main_module_block_format = Mb_struct { mb_repr = repr };
      compilation_unit;
      required_globals;
    }
  in
  match !jit with
  | None -> default_load ppf program
  | Some {Jit.load; _} -> load ~phrase_name:!phrase_name ppf program

let outval_of_id env id val_lpoly val_type =
  let glob, pos, (repr : Lambda.module_representation) = toplevel_value id in
  match mod_field (global_symbol glob) repr pos with
  | Some obj_to_print -> outval_of_value env obj_to_print val_lpoly val_type
  | None -> Oval_stuff "<abstr>"

(* Print the outcome of an evaluation *)

let pr_item =
  Out_type.print_items
    (fun env -> function
      | Sig_value(id, {val_kind = Val_reg _; val_type; val_lpoly}, _) ->
         Some (outval_of_id env id val_lpoly val_type)
      | _ -> None
    )

(* The current typing environment for the toplevel *)

let toplevel_env = ref Env.empty
let toplevel_sig = ref []

(* Print an exception produced by an evaluation *)

let print_out_exception ppf exn outv =
  !print_out_phrase ppf (Ophr_exception (exn, outv))

let print_exception_outcome ppf exn =
  if exn = Out_of_memory then Gc.full_major ();
  let outv =
    outval_of_value !toplevel_env (Obj.repr exn) (Lpoly.determined [])
      Predef.type_exn
  in
  print_out_exception ppf exn outv

(* The table of toplevel directives.
   Filled by functions from module topdirs. *)

let directive_table = (Hashtbl.create 23 : (string, directive_fun) Hashtbl.t)

let directive_info_table =
  (Hashtbl.create 23 : (string, directive_info) Hashtbl.t)

let add_directive name dir_fun dir_info =
  Hashtbl.add directive_table name dir_fun;
  Hashtbl.add directive_info_table name dir_info

(* Give a name to an unnamed expression *)

let name_expression ~loc ~attrs sort exp =
  let name = "_$" in
  let id = Ident.create_local name in
  let vd =
    { val_type = exp.exp_type;
      val_kind = Val_reg (Representable sort);
      val_loc = loc;
      val_attributes = attrs;
      val_zero_alloc = Zero_alloc.default;
      val_modalities = Mode.Modality.(Const.id |> of_const);
      val_uid = Uid.internal_not_actually_unique;
      val_lpoly = Lpoly.determined []; }
  in
  let sg = [Sig_value(id, vd, Exported)] in
  let pat =
    { pat_desc =
        Tpat_var { id; name = mknoloc name; uid = vd.val_uid; sort;
                   mode =
                      Mode.With_regionality.disallow_right
                        Mode.With_regionality.legacy };
      pat_loc = loc;
      pat_extra = [];
      pat_type = exp.exp_type;
      pat_env = exp.exp_env;
      pat_attributes = [];
      pat_unique_barrier = Unique_barrier.not_computed () }
  in
  let vb =
    { vb_pat = pat;
      vb_expr = exp;
      vb_rec_kind = Dynamic;
      vb_attributes = attrs;
      vb_loc = loc;
      vb_sort = sort }
  in
  let item =
    { str_desc = Tstr_value(Nonrecursive, [vb]);
      str_loc = loc;
      str_env = exp.exp_env; }
  in
  let final_env =
     Env.add_value ~mode:Mode.With_regionality.legacy id vd exp.exp_env
   in
  let str =
    { str_items = [item];
      str_type = sg;
      str_final_env = final_env }
  in
  str, sg

(* Execute a toplevel phrase *)

let execute_phrase print_outcome ppf phr =
  match phr with
  | Ptop_def sstr ->
      let oldenv = !toplevel_env in
      let oldsig = !toplevel_sig in
      incr phrase_seqid;
      phrase_name := Printf.sprintf "TOP%i" !phrase_seqid;
      let compilation_unit =
        Compilation_unit.create Compilation_unit.Prefix.empty
          (!phrase_name |> Compilation_unit.Name.of_string)
      in
      let unit_info =
        Unit_info.make_dummy ~input_name:!phrase_name compilation_unit
      in
      Compilenv.reset unit_info;
      Typecore.reset_delayed_checks ();
      let (str, sg, names, _shape, newenv) =
        Typemod.type_toplevel_phrase oldenv oldsig sstr
      in
      if !Clflags.dump_typedtree then Printtyped.implementation ppf str;
      let sg' = Typemod.Signature_names.simplify newenv names sg in
      let modes = Includemod.modes_toplevel in
      let coercion =
        Includemod.signatures oldenv ~mark:true ~modes sg sg'
      in
      Typecore.force_delayed_checks ();
      let str, sg', rewritten =
        match str.str_items with
        | [ { str_desc = Tstr_eval (e, sort, attrs) ; str_loc = loc } ]
        | [ { str_desc = Tstr_value (Asttypes.Nonrecursive,
                                      [{ vb_expr = e
                                       ; vb_pat =
                                           { pat_desc = Tpat_any;
                                             _ }
                                       ; vb_attributes = attrs
                                       ; vb_sort = sort }])
            ; str_loc = loc }
          ] ->
            let str, sg' = name_expression ~loc ~attrs sort e in
            str, sg', true
        | _ -> str, sg', false
      in
      let compilation_unit, res, required_globals, repr =
        let { Lambda.compilation_unit; main_module_block_format;
              required_globals; code = res } =
          Translmod.transl_implementation compilation_unit
            (str, coercion, None) ~loc:Location.none
        in
        let repr =
          match main_module_block_format with
          | Mb_struct { mb_repr } -> mb_repr
          | Mb_instantiating_functor _ ->
            Misc.fatal_error "Unexpected parameterised module in toplevel"
        in
        remember compilation_unit sg' repr;
        compilation_unit, close_phrase res, required_globals, repr
      in
      Warnings.check_fatal ();
      begin try
        toplevel_env := newenv;
        toplevel_sig := List.rev_append sg' oldsig;
        let res =
          load_tlambda ppf ~required_globals ~compilation_unit res repr
        in
        let out_phr =
          match res with
          | Result _ ->
              Env.register_import_as_opaque
                (Compilation_unit.name compilation_unit);
              if print_outcome then
                Printtyp.wrap_printing_env ~error:false oldenv (fun () ->
                match str.str_items with
                | [] -> Ophr_signature []
                | _ ->
                    if rewritten then
                      match sg' with
                      | [ Sig_value (id, vd, _) ] ->
                          let outv =
                            outval_of_id newenv id vd.val_lpoly vd.val_type
                          in
                          (* CR-someday zqian: should pass val_lpoly to
                             printer *)
                          let ty = Out_type.tree_of_type_scheme vd.val_type in
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
                outval_of_value !toplevel_env (Obj.repr exn)
                  (Lpoly.determined []) Predef.type_exn
              in
              Ophr_exception (exn, outv)
        in
        begin match out_phr with
        | Ophr_signature [] -> ()
        | _ ->
            Location.separate_new_message ppf;
            !print_out_phrase ppf out_phr
        end;
        begin match out_phr with
        | Ophr_eval (_, _) | Ophr_signature _ -> true
        | Ophr_exception _ -> false
        end
      with x ->
        toplevel_env := oldenv; toplevel_sig := oldsig; raise x
      end
  | Ptop_dir {pdir_name = {Location.txt = dir_name; _}; pdir_arg; _ } ->
      let d =
        try Some (Hashtbl.find directive_table dir_name)
        with Not_found -> None
      in
      begin match d with
      | None ->
          fprintf ppf "Unknown directive `%s'.@." dir_name;
          false
      | Some d ->
          match d, pdir_arg with
          | Directive_none f, None -> f (); true
          | Directive_string f, Some {pdira_desc = Pdir_string s; _} ->
              f s; true
          | Directive_int f, Some {pdira_desc = Pdir_int (n,None); _} ->
             begin match Int_literal_converter.int n with
             | n -> f n; true
             | exception _ ->
               fprintf ppf "Integer literal exceeds the range of \
                            representable integers for directive `%s'.@."
                       dir_name;
               false
             end
          | Directive_int _, Some {pdira_desc = Pdir_int (_, Some _); _} ->
              fprintf ppf "Wrong integer literal for directive `%s'.@."
                dir_name;
              false
          | Directive_ident f, Some {pdira_desc = Pdir_ident lid; _} ->
              f lid; true
          | Directive_bool f, Some {pdira_desc = Pdir_bool b; _} ->
              f b; true
          | _ ->
              fprintf ppf "Wrong type of argument for directive `%s'.@."
                dir_name;
              false
      end

(* Read and execute commands from a file, or from stdin if [name] is "". *)

let use_print_results = ref true

let preprocess_phrase ppf phr =
  let phr =
    match phr with
    | Ptop_def str ->
        let str =
          Pparse.apply_rewriters_str ~restore:true ~tool_name:"ocaml" str
        in
        Ptop_def str
    | phr -> phr
  in
  if !Clflags.dump_parsetree then Printast.top_phrase ppf phr;
  if !Clflags.dump_source then Pprintast.top_phrase ppf phr;
  phr

let use_channel ppf ~wrap_in_module ic name filename =
  let lb = Lexing.from_channel ic in
  Location.init lb filename;
  (* Skip initial #! line if any *)
  Lexer.skip_hash_bang lb;
  let success =
    Lexer.protect_syntax_mode @@ fun () ->
    protect_refs [ R (Location.input_name, filename) ] (fun () ->
      try
        List.iter
          (fun ph ->
            let ph = preprocess_phrase ppf ph in
            let success = execute_phrase !use_print_results ppf ph in
            if not success then raise Exit)
          (if wrap_in_module then
             parse_mod_use_file name lb
           else
             !parse_use_file lb);
        true
      with
      | Exit -> false
      | Sys.Break -> fprintf ppf "Interrupted.@."; false
      | x -> Location.report_exception ppf x; false) in
  success

let use_output ppf command =
  let fn = Filename.temp_file "ocaml" "_toploop.ml" in
  Misc.try_finally ~always:(fun () ->
      try Sys.remove fn with Sys_error _ -> ())
    (fun () ->
       match
         Printf.ksprintf Sys.command "%s > %s"
           command
           (Filename.quote fn)
       with
       | 0 ->
         let ic = open_in_bin fn in
         Misc.try_finally ~always:(fun () -> close_in ic)
           (fun () ->
              use_channel ppf ~wrap_in_module:false ic "" "(command-output)")
       | n ->
         fprintf ppf "Command exited with code %d.@." n;
         false)

let use_file ppf ~wrap_in_module name =
  match name with
  | "" ->
    use_channel ppf ~wrap_in_module stdin name "(stdin)"
  | _ ->
    match Load_path.find name with
    | filename ->
      let ic = open_in_bin filename in
      Misc.try_finally ~always:(fun () -> close_in ic)
        (fun () -> use_channel ppf ~wrap_in_module ic name filename)
    | exception Not_found ->
      fprintf ppf "Cannot find file %s.@." name;
      false

let mod_use_file ppf name =
  use_file ppf ~wrap_in_module:true name
let use_file ppf name =
  use_file ppf ~wrap_in_module:false name

let use_silently ppf name =
  protect_refs [ R (use_print_results, false) ] (fun () -> use_file ppf name)

(* Reading function for interactive use *)

let first_line = ref true
let got_eof = ref false;;

let read_input_default prompt buffer len =
  output_string stdout prompt; flush stdout;
  let i = ref 0 in
  try
    while true do
      if !i >= len then raise Exit;
      let c = input_char stdin in
      Bytes.set buffer !i c;
      incr i;
      if c = '\n' then raise Exit;
    done;
    (!i, false)
  with
  | End_of_file ->
      (!i, true)
  | Exit ->
      (!i, false)

let read_interactive_input = ref read_input_default

let refill_lexbuf buffer len =
  if !got_eof then (got_eof := false; 0) else begin
    let prompt =
      if !Clflags.noprompt then ""
      else if !first_line then "# "
      else if !Clflags.nopromptcont then ""
      else if Lexer.in_comment () then "* "
      else "  "
    in
    first_line := false;
    let (len, eof) = !read_interactive_input prompt buffer len in
    if eof then begin
      Location.echo_eof ();
      if len > 0 then got_eof := true;
      len
    end else
      len
  end

(* Toplevel initialization. Performed here instead of at the
   beginning of loop() so that user code linked in with ocamlmktop
   can call directives from Topdirs. *)

let _ =
  Sys.interactive := true;
  (* Phrases are compiled into this executable's own image, so the variant
     flag defaults must describe the variant it was built for rather than
     the installation default from [Config].  They are set before the load
     path is first computed because [set_paths] keeps that path's implicit
     standard-library entry, against which command-line objects such as
     "str/str.cmxs" are then found: it has to be a root that serves the host
     variant. *)
  Codegen_variant.apply host_variant;
  Compmisc.init_path ();
  Clflags.dlcode := true;
  ()

(* The implicit entries of that first load path: the one for the current
   directory, which comes first, and the one for the standard library. *)
let initial_implicit_entries =
  (match (Load_path.get_paths ()).visible with
   | [] -> []
   | first :: _ -> [first.path])
  @ Clflags.std_include_dir ()

(* The implicit entry of a load path for the current directory, as
   [Compmisc.init_path] makes it for the interactive loop: none under
   -nocwd, and the counterpart of the directory in the library tree of the
   selected variant where it has one. It is computed when the path is made:
   after [#cd] it stands for what it stood for then, as the entry of the
   interactive loop does. *)
let current_directory_entry () : Clflags.include_dir list =
  if !Clflags.no_cwd then []
  else
    [ { path = Codegen_variant.in_selected_root ""; cmx_guaranteed = false } ]

let find_ocamlinit () =
  let ocamlinit = ".ocamlinit" in
  if Sys.file_exists ocamlinit then Some ocamlinit else
  let getenv var = match Sys.getenv var with
    | exception Not_found -> None | "" -> None | v -> Some v
  in
  let exists_in_dir dir file = match dir with
    | None -> None
    | Some dir ->
        let file = Filename.concat dir file in
        if Sys.file_exists file then Some file else None
  in
  let home_dir () = getenv "HOME" in
  let config_dir () =
    if Sys.win32 then None else
    match getenv "XDG_CONFIG_HOME" with
    | Some _ as v -> v
    | None ->
        match home_dir () with
        | None -> None
        | Some dir -> Some (Filename.concat dir ".config")
  in
  let init_ml = Filename.concat "ocaml" "init.ml" in
  match exists_in_dir (config_dir ()) init_ml with
  | Some _ as v -> v
  | None -> exists_in_dir (home_dir ()) ocamlinit

let load_ocamlinit ppf =
  if !Clflags.noinit then ()
  else match !Clflags.init_file with
  | Some f -> if Sys.file_exists f then ignore (use_silently ppf f)
              else fprintf ppf "Init file not found: \"%s\".@." f
  | None ->
      match find_ocamlinit () with
      | None -> ()
      | Some file -> ignore (use_silently ppf file)
;;

let set_paths () =
  (* Add whatever -I options have been specified on the command line,
     but keep the directories that user code linked in with ocamlmktop
     may have added to load_path. *)
  (* ocamlnat's variant is pinned to the binary's own build variant (see
     [host_variant] and [prepare]), so this resolves against that
     variant's library root.  The -I/-H directories were asked for by the
     user and get the checked expansion; "+camlp4" is our own optional
     addition, which must not become an error when that root lacks it. *)
  let expand = Codegen_variant.expand_directory in
  let expand_entry (e : Clflags.include_dir) : Clflags.include_dir =
    { path = expand e.path; cmx_guaranteed = e.cmx_guaranteed }
  in
  let include_no_cmx path = { Clflags.path ; cmx_guaranteed = false } in
  let Load_path.{ visible; hidden } = Load_path.get_paths () in
  (* The implicit entries of the first load path date from before the
     command line was read, and are made again from the flags. The flags
     for poll insertion may have selected the other of the two library
     trees that serve this executable, so that an object given by name on
     the command line is found where the same object given by path is, and
     where a directive of the interactive loop finds it; -nocwd and
     -nostdlib take their entries away, as for a compiler. What remains of
     the first path is what user code added to it. Each implicit entry is
     taken out once, by what it was made for: the two can name the same
     directory. *)
  let rec without_one path = function
    | [] -> []
    | (e : Clflags.include_dir) :: entries ->
      if String.equal e.path path then entries
      else e :: without_one path entries
  in
  let added =
    List.fold_left
      (fun entries path -> without_one path entries)
      visible initial_implicit_entries
  in
  let standard_library =
    List.map
      (fun path -> { Clflags.path; cmx_guaranteed = true })
      (Clflags.std_include_dir ())
  in
  let camlp4 =
    if !Clflags.no_std_include then []
    else
      [ include_no_cmx (Codegen_variant.expand_directory_unchecked "+camlp4") ]
  in
  let visible = List.concat [
      current_directory_entry ();
      List.map expand_entry (List.rev !Compenv.first_include_dirs);
      List.map expand_entry (List.rev !Clflags.include_dirs);
      List.map expand_entry (List.rev !Compenv.last_include_dirs);
      added;
      standard_library;
      camlp4;
    ]
  in
  let hidden = List.concat [
      List.map expand_entry (List.rev !Clflags.hidden_include_dirs);
      hidden
    ]
  in
  Load_path.init ~auto_include:Compmisc.auto_include ~visible ~hidden

let initialize_toplevel_env () =
  toplevel_env := Compmisc.initial_env();
  toplevel_sig := []

(* The interactive loop *)

exception PPerror

let reset_location = ref true

let loop ppf =
  Location.formatter_for_warnings := ppf;
  if not !Clflags.noversion then
    fprintf ppf "        OCaml version %s - native toplevel@.@." Config.version;
  initialize_toplevel_env ();
  let lb = Lexing.from_function refill_lexbuf in
  Location.init lb "//toplevel//";
  Location.input_name := "//toplevel//";
  Location.input_lexbuf := Some lb;
  Sys.catch_break true;
  run_hooks After_setup;
  load_ocamlinit ppf;
  while true do
    let snap = Btype.snapshot () in
    try
      if !reset_location then begin
        Lexing.flush_input lb;
        Location.reset();
      end;
      first_line := true;
      let phr = try !parse_toplevel_phrase lb with Exit -> raise PPerror in
      let phr = preprocess_phrase ppf phr  in
      Env.reset_cache_toplevel ();
      ignore(execute_phrase true ppf phr)
    with
    | End_of_file -> raise (Compenv.Exit_with_status 0)
    | Sys.Break -> fprintf ppf "Interrupted.@."; Btype.backtrack snap
    | PPerror -> ()
    | x -> Location.report_exception ppf x; Btype.backtrack snap
  done

external caml_sys_modify_argv : string array -> unit =
  "caml_sys_modify_argv"

let override_sys_argv new_argv =
  caml_sys_modify_argv new_argv;
  Arg.current := 0

(* Execute a script.  If [name] is "", read the script from stdin. *)

let run_script ppf name args =
  override_sys_argv args;
  Compmisc.init_path ~dir:(Filename.dirname name) ();
                   (* Note: would use [Filename.abspath] here, if we had it. *)
  initialize_toplevel_env ();
  Sys.interactive := false;
  run_hooks After_setup;
  let explicit_name =
    (* Prevent use_silently from searching in the path. *)
    if Filename.is_implicit name
    then Filename.concat Filename.current_dir_name name
    else name
  in
  use_silently ppf explicit_name


module Compiler = (val Optcompile.native
                   (module Unix_for_owee : Compiler_owee.Unix_intf.S)
                   ~flambda2:Flambda2.lambda_to_cmm)

(* Load in-core a .cmxs file *)

let linkenv = Linkenv.create ()

let load_file ppf name0 =
  let name =
    try Some (Compmisc.find_input name0)
    with Not_found -> None
  in
  match name with
  | None -> fprintf ppf "File not found: %s@." name0; false
  | Some name ->
    let fn,tmp =
      if Filename.check_suffix name Compiler.ext_flambda_obj
         || Filename.check_suffix name Compiler.ext_flambda_lib
      then
        let cmxs = Filename.temp_file "caml" ".cmxs" in
        (* The file has been found: the linker must not search for it
           again, as it would for a name without a directory. *)
        let file =
          if Filename.is_implicit name
          then Filename.concat Filename.current_dir_name name
          else name
        in
        Compiler.link_shared ~ppf_dump:ppf linkenv [file] cmxs;
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

let preload_objects = ref []

let prepare ppf ?input:_ () =
  (* Compiled phrases are loaded into the toplevel's own image, so they must
     use the whole-program axes of the codegen variant the toplevel itself
     was built for (not necessarily the install default; see [host_variant]
     above).  Once this pin passes, the current flags select a variant that
     the library trees of [host_variant] serve, so [set_paths]'s
     [Codegen_variant.expand_directory] resolves the load path against such
     a tree too. *)
  let current_variant = Codegen_variant.flags_variant () in
  if not (variant_matches_host current_variant) then
    Compenv.fatal
      (Printf.sprintf
         "ocamlnat was built for codegen variant '%s' and cannot compile \
          phrases for variant '%s'; adjust the variant-selecting flags to \
          match the variant ocamlnat was built for"
         (Codegen_variant.to_string host_variant)
         (Codegen_variant.to_string current_variant));
  (* Poll insertion is not part of that pin, but every phrase would fail
     the check of its own: say so now, in terms of what ocamlnat was
     given. *)
  (try check_poll_insertion_for_in_process_load ()
   with Failure _ ->
     Compenv.fatal
       (if !Clflags.poll_insertion
        then
          "ocamlnat is fully polled and may run more than one domain, so it \
           cannot load phrases compiled by the LLVM backend, which inserts \
           no polls; do not pass -llvm-backend"
        else
          "ocamlnat is fully polled and may run more than one domain, so it \
           cannot load phrases compiled without poll insertion; do not pass \
           -disable-poll-insertion, or run it with OCAMLRUNPARAM=d=1"));
  set_paths ();
  begin try
    initialize_toplevel_env ()
  with Env.Error.In_context _ | Typetexp.Error.In_context _ as exn ->
    Location.report_exception ppf exn; raise (Compenv.Exit_with_status 2)
  end;
  try
    let res =
      let objects =
        List.rev (!preload_objects @ !Compenv.first_objfiles)
      in
      List.for_all (load_file ppf) objects
    in
    run_hooks Startup;
    res
  with x ->
    try Location.report_exception ppf x; false
    with x ->
      Format.fprintf ppf "Uncaught exception: %s\n" (Printexc.to_string x);
      false
