(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Flags used in OCaml commands *)

(* The variant that the compilers compile for when nothing selects one,
   and that the library root of every install holds: base. *)
let base_codegen_variant =
  Codegen_variant_name.to_string Codegen_variant_name.base

let codegen_variant_directory name =
  if String.equal name base_codegen_variant
  then Ocaml_directories.stdlib
  else
    Ocamltest_stdlib.Filename.make_path
      [Ocaml_directories.stdlib; "variants"; name]

(* Whether the tested install can compile and link native code for the named
   codegen variant: either the variant is base (served by the
   standard-library root itself), or the install provides a library tree
   for it under <stdlib>/variants/.  Checked against the actual tree
   being tested rather than configure-time truth, so that predicates stay
   correct if the variant trees are absent (e.g. --disable-codegen-variants
   builds, or incomplete installs). *)
let codegen_variant_available name =
  String.equal name base_codegen_variant
  || (let dir = codegen_variant_directory name in
      Sys.file_exists dir && Sys.is_directory dir)

let available_codegen_variants =
  let available =
    lazy
      (let variants =
         Ocamltest_stdlib.Filename.make_path
           [Ocaml_directories.stdlib; "variants"]
       in
       let installed =
         if Sys.file_exists variants && Sys.is_directory variants
         then
           List.filter codegen_variant_available
             (List.sort String.compare (Array.to_list (Sys.readdir variants)))
         else []
       in
       base_codegen_variant :: installed)
  in
  fun () -> Lazy.force available

(* A codegen variant (absolute name, e.g. "base" or "fp"; see
   utils/codegen_variant_name.ml, copied into ocamltest) is selected either
   by a test, with the [codegen_variant] TSL variable, or for the whole run,
   with the -codegen-variant option (the "suite variant").  The test's
   selection wins; an empty [codegen_variant] selects nothing, so the suite
   variant applies.

   Asking for the base variant is the same as not asking:
   [make test CODEGEN_VARIANT=base] is [make test]. *)
let suite_codegen_variant =
  match Options.codegen_variant with
  | "" -> None
  | name when String.equal name base_codegen_variant -> None
  | name -> Some name

(* A value of [codegen_variant] that is neither a name nor requirements is
   an error of the test, reported wherever the selection is read: a
   misspelled selection must look neither like a variant that the install
   lacks nor like no selection at all.  The exception carries the message,
   which starts with the same words whatever the [reason]. *)
exception Invalid_codegen_variant of string

let invalid_codegen_variant ?reason selection =
  let message =
    Printf.sprintf
      "codegen_variant \"%s\" is neither the name of a codegen variant (such \
       as \"base\" or \"fp-sc\") nor requirements, which are signed axes \
       (such as \"+fp-sc\" for fp on and sc off)"
      selection
  in
  raise
    (Invalid_codegen_variant
       (match reason with
        | None -> message
        | Some reason -> message ^ ": " ^ reason))

(* The variant that [name] names.  Every name that ocamltest computes with
   is one: the base variant, the suite variant (checked at
   startup) or a test's selection (checked by [test_codegen_variant]). *)
let variant_of_name name =
  match Codegen_variant_name.of_string name with
  | Some variant -> variant
  | None -> invalid_codegen_variant name

(* A test selects a variant by its name ("fp-sc"), or by what it needs of
   one: signed axes, +<axis> (the axis is on) and -<axis> (it is off), as
   in "+fp" or "+fp-sc".  The syntax is that of the warning options of the
   compilers (-w +a-4): a value whose first character other than a blank
   is a sign is requirements, and is split at every sign and at blanks, so
   that "+fp-sc" and "+fp -sc" are the same.  Each axis is given once. *)
let is_blank c = Char.equal c ' ' || Char.equal c '\t'

let is_sign c = Char.equal c '+' || Char.equal c '-'

let is_requirements selection =
  let length = String.length selection in
  let rec first_character i =
    i < length
    && (is_sign selection.[i]
        || (is_blank selection.[i] && first_character (i + 1)))
  in
  first_character 0

let requirements_of_selection selection =
  let invalid reason = invalid_codegen_variant ~reason selection in
  let length = String.length selection in
  let rec word_end i =
    if i < length && not (is_sign selection.[i] || is_blank selection.[i])
    then word_end (i + 1)
    else i
  in
  let rec requirements_from i requirements =
    if i >= length
    then List.rev requirements
    else if is_blank selection.[i]
    then requirements_from (i + 1) requirements
    else if is_sign selection.[i]
    then begin
      let j = word_end (i + 1) in
      let name = String.sub selection (i + 1) (j - i - 1) in
      match Codegen_variant_name.token_of_name name with
      | Some token ->
        if List.exists
             (fun (other, _) -> Codegen_variant_name.equal_token token other)
             requirements
        then invalid (Printf.sprintf "the axis %s is given twice" name)
        else
          requirements_from j
            ((token, Char.equal selection.[i] '+') :: requirements)
      | None ->
        invalid
          (if String.equal name ""
           then "a sign is followed by the name of an axis"
           else if Option.is_some (Codegen_variant_name.of_string name)
           then
             Printf.sprintf "\"%s\" is the name of a variant, not an axis"
               name
           else
             Printf.sprintf "\"%s\" is not an axis (the axes are %s)" name
               (String.concat ", "
                  (List.map Codegen_variant_name.token_name
                     Codegen_variant_name.all_tokens)))
    end
    else
      let j = word_end i in
      invalid
        (Printf.sprintf "\"%s\" has no sign" (String.sub selection i (j - i)))
  in
  requirements_from 0 []

(* A value that does not start with a sign is the name of a variant.  One
   that has a sign or a blank further on is likely a slip between the two
   forms, so the error shows what each form would be. *)
let variant_of_selection selection =
  match Codegen_variant_name.of_string selection with
  | Some variant -> variant
  | None ->
    if String.exists (fun c -> Char.equal c '+' || is_blank c) selection
    then
      let requirement = "+" ^ selection in
      invalid_codegen_variant selection
        ~reason:
          (match requirements_of_selection requirement with
           | requirements ->
             Printf.sprintf
               "requirements start with a sign, blanks aside (\"%s\" asks \
                for %s); a name has no sign in front, no blank and no \"+\" \
                (\"%s\" is the name of the variant with these axes on)"
               requirement
               (String.concat ", "
                  (List.map
                     (fun (token, enabled) ->
                       Codegen_variant_name.token_name token
                       ^ if enabled then " on" else " off")
                     requirements))
               (Codegen_variant_name.to_string
                  (Codegen_variant_name.of_tokens (List.map fst requirements)))
           | exception Invalid_codegen_variant _ ->
             "requirements start with a sign, blanks aside; a name has no \
              sign in front, no blank and no \"+\"")
    else invalid_codegen_variant selection

(* A test that is about an axis, and not about one variant, states its
   requirements.  It then runs under the variant that the run uses anyway
   (the suite variant, else base) when that one meets
   them, and otherwise under the available variant that meets them and
   differs from it on the fewest axes (the first such one in
   [available_codegen_variants]).  So it runs wherever some variant will
   do, whatever the default and the list of variants of the install, and
   under each suite variant that will do.  When no available variant meets
   the requirements, the result is the one that would have been closest,
   for [variant_available] to report. *)
let variant_meeting requirements =
  let run_variant =
    variant_of_name
      (Option.value suite_codegen_variant ~default:base_codegen_variant)
  in
  let required token =
    List.find_map
      (fun (required, enabled) ->
        if Codegen_variant_name.equal_token required token
        then Some enabled
        else None)
      requirements
  in
  let meets variant =
    List.for_all
      (fun (token, enabled) ->
        Bool.equal (Codegen_variant_name.mem token variant) enabled)
      requirements
  in
  let distance variant =
    List.length
      (List.filter
         (fun token ->
           not
             (Bool.equal
                (Codegen_variant_name.mem token variant)
                (Codegen_variant_name.mem token run_variant)))
         Codegen_variant_name.all_tokens)
  in
  let closest best name =
    match Codegen_variant_name.of_string name with
    | Some variant when meets variant ->
      (match best with
       | Some (_, best_distance) when best_distance <= distance variant ->
         best
       | Some _ | None -> Some (name, distance variant))
    | Some _ | None -> best
  in
  match List.fold_left closest None (available_codegen_variants ()) with
  | Some (name, _) -> name
  | None ->
    Codegen_variant_name.to_string
      (Codegen_variant_name.of_tokens
         (List.filter
            (fun token ->
              Option.value (required token)
                ~default:(Codegen_variant_name.mem token run_variant))
            Codegen_variant_name.all_tokens))

let test_codegen_variant env =
  match Environments.safe_lookup Ocaml_variables.codegen_variant env with
  | "" -> None
  | selection ->
    if is_requirements selection
    then Some (variant_meeting (requirements_of_selection selection))
    else
      Some (Codegen_variant_name.to_string (variant_of_selection selection))

(* The selected variant is what everything that generates or loads native
   code targets: compilations and links, which get the variant's flags
   ([codegen_variant_flags]) and library tree ([stdlib],
   [variant_library_directories]), the programs that compile in process
   ([Ocaml_files.native_host]), C code built for native programs
   ([cflags]) and the commands that tests write themselves, with the
   compilers that they start ([command_environment]). *)
let codegen_variant env =
  match test_codegen_variant env with
  | Some _ as selection -> selection
  | None -> suite_codegen_variant

(* The codegen variant that a test block's native code is built for: the
   selected variant, or base, which every axis flag
   defaults to (per-test [flags +=] additions are not visible here). *)
let effective_codegen_variant env =
  match codegen_variant env with
  | None -> base_codegen_variant
  | Some name -> name

(* The effective variant when it is not base, the
   case in which commands need something more than their defaults. *)
let non_base_codegen_variant env =
  match codegen_variant env with
  | Some name when not (String.equal name base_codegen_variant) ->
    Some name
  | Some _ | None -> None

(* The variant whose library tree a compilation for [backend] uses.
   Bytecode has no codegen variants: the bytecode compilers take no axis
   flag, ignore the axis keys of OCAMLPARAM and always use the default
   tree, so bytecode steps run the same under every selection.  (The
   interfaces and the bytecode objects of a variant tree are those of the
   root, so one test block can still mix backends.)  The same
   holds for the JSIR targets. *)
let library_codegen_variant env (backend : Ocaml_backends.t) =
  match backend with
  | Native -> non_base_codegen_variant env
  | Bytecode | Jsir_js | Jsir_wasm -> None

(* Marks [task] as work that depends on a non-base codegen variant: a
   command that is given such a variant ([variant_work_for]), or what a
   guard lets through when it holds for the effective variant but not for
   base ([codegen_variant_predicate]).  Under a suite variant, a
   test file whose task carries no such mark would only repeat the default
   run, so [Main.test_file] skips it when the run is restricted to what
   depends on the variant (-codegen-variant-only).  The mark is part of the
   task, so it goes away with a branch that is dropped. *)
let variant_work task =
  Actions.seq [Actions.Task [Depends_on_codegen_variant]; task]

let has_variant_work (task : Actions.task) =
  match task with
  | Static _ -> false
  | Task items ->
    List.exists
      (fun (item : Actions.task_item) ->
        match item with
        | Depends_on_codegen_variant -> true
        | Predicate_pass _ | Chdir _ | Check_file_exists _
        | Check_file_absent _ | Copy_file _ | Setup_build_env _ | Run_cmd _
        | Compare_files _ -> false)
      items

(* Per-axis read of the effective variant, for the axis predicates
   (frame_pointers, stack-checks, poll-insertion, address-sanitizer and
   their duals): each predicate's meaning is the axis's value in the
   variant the block actually compiles for. *)
let codegen_variant_name_mem name token =
  Codegen_variant_name.mem token (variant_of_name name)

let effective_codegen_variant_mem env token =
  codegen_variant_name_mem (effective_codegen_variant env) token

(* A predicate on the effective variant: [holds] applied to its name.  When
   it holds for the effective variant but not for base, what it
   lets through is work that only this variant can do, whatever the backend
   of that work (see [variant_work]). *)
let codegen_variant_predicate holds pass_reason skip_reason log env =
  let satisfied = holds (effective_codegen_variant env) in
  let task, env =
    Actions_helpers.predicate satisfied pass_reason skip_reason log env
  in
  let task =
    if satisfied && not (holds base_codegen_variant)
    then variant_work task
    else task
  in
  task, env

let codegen_variant_axis_predicate token ~enabled =
  codegen_variant_predicate (fun name ->
    Bool.equal (codegen_variant_name_mem name token) enabled)

let codegen_variant_flags env backend =
  match backend, codegen_variant env with
  | Ocaml_backends.(Bytecode | Jsir_js | Jsir_wasm), _ | _, None -> ""
  | Ocaml_backends.Native, Some name ->
    String.concat " "
      (Codegen_variant_name.ocamlopt_flags (variant_of_name name))

(* [task] runs a command that is given the selected variant.  A variant
   that the install lacks can only be a test's own selection (the suite
   variant is checked at startup), in a block that [variant_available]
   does not guard: the command would run for another variant, or fail for
   a reason that does not say so. *)
let variant_work_for env task =
  match non_base_codegen_variant env with
  | Some name when codegen_variant_available name -> variant_work task
  | Some name ->
    Actions.Static
      (Result.fail_with_reason
         (Printf.sprintf
            "the compiler under test has no codegen variant %s (selected \
             by codegen_variant = \"%s\"): guard the block with \
             variant_available"
            name
            (Environments.safe_lookup Ocaml_variables.codegen_variant env)))
  | None -> task

(* [variant_work_for] for a compiler for [backend], which is given the
   variant through [codegen_variant_flags]. *)
let native_variant_work env (backend : Ocaml_backends.t) task =
  match backend with
  | Native -> variant_work_for env task
  | Bytecode | Jsir_js | Jsir_wasm -> task

(* The directories that tests name are those of the base variant:
   library modifiers (see ocaml_modifiers.ml, e.g. [include unix]) add
   <srcdir>/otherlibs/<lib>, [include testing] and [include tool-ocaml-lib]
   add <srcdir>/testsuite/lib, the compiler-libs modifiers ([include
   ocamlcommon] and the like, [include config]) add the directories of
   [Ocaml_modifiers.compilerlibs_subdirs], and tests also write such
   directories themselves.  A compilation for another variant must not mix
   the base variant's artifacts in (its C stub archives would link
   unnoticed), so each such directory is resolved in that variant's
   library tree instead (the read-time analogue of the [stdlib] remap
   below) whenever the tree provides it: the otherlibs are
   subdirectories of the tree (the systhreads source directory is named
   "systhreads", but it installs as "threads"), eval is at its root, the
   compiler's libraries are in its compiler-libs directory, and the
   testsuite's own libraries are under <srcdir>/testsuite/lib/variants (see
   [make install_for_test]).  Any other directory, and every directory
   when the variant is the default, is left unchanged. *)
let library_directory_for_variant name dir =
  if String.equal name base_codegen_variant then dir
  else begin
    let srcdir_path = Ocaml_directories.srcdir_path in
    let variant_path path =
      Ocamltest_stdlib.Filename.make_path
        (codegen_variant_directory name :: path)
    in
    let otherlibs = srcdir_path ["otherlibs"] ^ Filename.dir_sep in
    let testsuite_lib = srcdir_path ["testsuite"; "lib"] in
    let candidate =
      if String.equal dir testsuite_lib
      then
        Some
          (Ocamltest_stdlib.Filename.make_path
             [testsuite_lib; "variants"; name])
      else if List.exists
                (fun subdir -> String.equal dir (srcdir_path [subdir]))
                Ocaml_modifiers.compilerlibs_subdirs
      then Some (variant_path ["compiler-libs"])
      else
        match Ocamltest_stdlib.String.remove_prefix ~prefix:otherlibs dir with
        | Some "eval" -> Some (variant_path [])
        | Some "systhreads" -> Some (variant_path ["threads"])
        | Some rest -> Some (variant_path [rest])
        | None -> None
    in
    match candidate with
    | Some candidate
      when Sys.file_exists candidate && Sys.is_directory candidate ->
      candidate
    | Some _ | None -> dir
  end

(* [dirs], the value of the [directories] variable, resolved in the library
   tree that a compilation for [backend] uses.  Several directories of the
   base variant can be one directory of another variant, which is then
   named once. *)
let variant_library_directories env ~backend dirs =
  match library_codegen_variant env backend with
  | None -> dirs
  | Some name ->
    List.fold_left
      (fun dirs dir ->
        let dir = library_directory_for_variant name dir in
        if List.mem dir dirs then dirs else dir :: dirs)
      [] dirs
    |> List.rev

(* [flags], the value of [flags] or of [ocamlopt_flags], with the
   directories that it gives to -I resolved like those of [directories].
   A command line is split into words at the spaces that are not inside
   quotes ([Ocamltest_stdlib.String.words]), so a directory is the word
   after the word -I.  Everything else, quoted words included, is copied
   as it is. *)
let variant_include_flags env ~backend flags =
  match library_codegen_variant env backend with
  | None -> flags
  | Some name ->
    let length = String.length flags in
    let resolved = Buffer.create length in
    (* The end of the word that starts at [i], and whether quotes are part
       of it. *)
    let rec word_end i ~quote ~quoted =
      if i >= length then i, quoted
      else
        match flags.[i], quote with
        | ' ', None -> i, quoted
        | ('\'' | '"') as c, None ->
          word_end (i + 1) ~quote:(Some c) ~quoted:true
        | c, Some quote_char when Char.equal c quote_char ->
          word_end (i + 1) ~quote:None ~quoted
        | _, (Some _ | None) -> word_end (i + 1) ~quote ~quoted
    in
    let rec copy i ~after_include =
      if i < length
      then
        match flags.[i] with
        | ' ' ->
          Buffer.add_char resolved ' ';
          copy (i + 1) ~after_include
        | _ ->
          let j, quoted = word_end i ~quote:None ~quoted:false in
          let word = String.sub flags i (j - i) in
          Buffer.add_string resolved
            (if after_include && not quoted
             then library_directory_for_variant name word
             else word);
          copy j ~after_include:(String.equal word "-I")
    in
    copy 0 ~after_include:false;
    Buffer.contents resolved

let stdlib env ~backend =
  let stdlib_path =
    match library_codegen_variant env backend with
    | None -> Ocaml_directories.stdlib
    | Some name -> codegen_variant_directory name
  in
  "-nostdlib -I " ^ stdlib_path

(* [stdlib] is the library tree of the effective variant, in every block:
   the bytecode files are the same in every tree, and a block set up for a
   bytecode compiler may start native compilers too.  The variable follows
   the selection through [codegen_variant_stdlib]: it starts out as a
   reference to that variable ([variant_stdlib_reference]), which is bound
   again before every test and action of a test file
   ([Main.run_test_tree]).  A test that assigns [stdlib] replaces the
   reference, so its value stays, whatever it is.  A selection that is
   not valid is left for what reads it next to report. *)
let variant_stdlib_reference =
  Printf.sprintf "${%s}"
    (Variables.name_of_variable Ocaml_variables.codegen_variant_stdlib)

let with_variant_stdlib env =
  match effective_codegen_variant env with
  | name ->
    Environments.add Ocaml_variables.codegen_variant_stdlib
      (codegen_variant_directory name) env
  | exception Invalid_codegen_variant _ -> env

(* The C code that a test builds itself, with the [cc] action or from a
   script, goes into the programs of its block.  Native programs are built
   for the effective variant, and so is their C code.  Bytecode and JSIR
   programs have no codegen variant: their C code is built for base,
   like the bytecode runtime that it is linked with.  A block builds
   native programs unless it was set up for another backend
   ([set_build_env_backend]). *)
let set_build_env_backend backend env =
  Environments.add Ocaml_variables.build_env_backend
    (Ocaml_backends.string_of_backend backend)
    env

let builds_native_programs env =
  match Environments.lookup Ocaml_variables.build_env_backend env with
  | None -> true
  | Some backend ->
    String.equal backend (Ocaml_backends.string_of_backend Native)

(* [variant_work_for] for a task that builds C code of the block: it is
   given the variant where that code goes into native programs, and has
   nothing to do with it elsewhere. *)
let c_code_variant_work env task =
  if builds_native_programs env then variant_work_for env task else task

(* ocamltest gives the compilers that a command starts their variant in
   OCAMLPARAM ([command_environment]), and a command may start another
   ocamltest: a test of ocamltest itself, or of the testsuite's makefile.
   The axis keys before the '_' of the OCAMLPARAM that this ocamltest
   inherited are such a gift, to the command that started it: they are not
   settings for its tests.  Left there, they would come after the keys of
   a test's own variant and override them, and they would stay for a test
   that selects nothing, whose predicates and library tree are the
   base variant's.  [clear_inherited_axis_defaults] takes them out of
   the environment that every command of this run inherits.  Whatever else
   the value has stays.

   An axis key after the '_' is not such a gift: only a user writes one.
   The compilers obey it whatever their command line and the defaults
   say, so it would override the variant of every test, while the
   predicates, the library tree and the hosts follow the selection.
   [inherited_axis_settings] lists these settings, for ocamltest to
   refuse them before it runs anything. *)
let is_axis_setting item =
  List.exists
    (fun token ->
      let setting = Codegen_variant_name.ocamlparam token ~enabled:true in
      let key = String.sub setting 0 (String.index setting '=' + 1) in
      String.starts_with ~prefix:key item)
    Codegen_variant_name.all_tokens

(* The items of a value of OCAMLPARAM, with the separator that it starts
   with if it does: the first item is then empty. *)
let ocamlparam_items value =
  if String.equal value ""
  then ",", []
  else
    match value.[0] with
    | (':' | '|' | ';' | ' ' | ',') as separator ->
      String.make 1 separator, String.split_on_char separator value
    | _ -> ",", String.split_on_char ',' value

let without_axis_defaults value =
  let rec defaults = function
    | [] -> []
    | "_" :: _ as rest -> rest
    | item :: rest ->
      if is_axis_setting item then defaults rest else item :: defaults rest
  in
  let separator, items = ocamlparam_items value in
  String.concat separator (defaults items)

let inherited_ocamlparam () = Ocamltest_stdlib.Sys.safe_getenv "OCAMLPARAM"

let clear_inherited_axis_defaults () =
  let inherited = inherited_ocamlparam () in
  let cleared = without_axis_defaults inherited in
  if not (String.equal cleared inherited)
  then Unix.putenv "OCAMLPARAM" cleared

let inherited_axis_settings () =
  let rec after_underscore = function
    | [] -> []
    | "_" :: rest -> List.filter is_axis_setting rest
    | _ :: rest -> after_underscore rest
  in
  after_underscore (snd (ocamlparam_items (inherited_ocamlparam ())))

(* What a C compiler needs after the configured flags to compile the C
   code of the block against the installed headers, which describe the
   base variant: for native programs under another variant, the marker
   that the axis macros are given explicitly, the macros of the enabled
   axes, and the flags that change the generated code (for
   AddressSanitizer, also needed at the link).  Nothing otherwise. *)
let variant_cc_flags env =
  match non_base_codegen_variant env with
  | Some name when builds_native_programs env ->
    String.concat " "
      (Codegen_variant_name.variant_cc_flags (variant_of_name name))
  | Some _ | None -> ""

let cflags env =
  match variant_cc_flags env with
  | "" -> Ocamltest_config.cflags
  | variant_flags -> Ocamltest_config.cflags ^ " " ^ variant_flags

(* [env] with [cflags] and [mkexe] followed by [variant_cc_flags]: what is
   built for another variant than base gets the two variables
   in this form, where the last word is the variant's.  The flags that a
   test gives a native compiler are read in such an environment too: the
   compiler states the variant itself for the C files that it compiles,
   but before the options of -ccopt, and not at all in what else the
   flags do with the two variables, such as a preprocessor given with
   -pp. *)
let with_variant_c_flags env =
  match variant_cc_flags env with
  | "" -> env
  | variant_flags ->
    env
    |> Environments.append Ocaml_variables.cflags (" " ^ variant_flags)
    |> Environments.append Ocaml_variables.mkexe (" " ^ variant_flags)

(* A command that a test writes itself (a script, a hook, the program of
   [run], a compiler given its whole command line, ocamlmklib) sees the
   install under test as one configured with the effective variant as its
   default.

   The compilers that it starts take the variant as their default through
   OCAMLPARAM: its keys go before the '_' of the value the command would
   otherwise see (the test's own, or the one ocamltest inherited), so they
   are defaults that the command's own flags still override.

   [cflags] and [mkexe], which tests use for the C code of their programs
   and for the links that ocamlopt leaves to them (such as the final link
   of an -output-complete-obj object, often compiling a C file in the same
   command), are followed by [variant_cc_flags].  The variables themselves
   keep what the test has in them, so the addition follows the selection
   and the block, and a test can change them. *)
let command_environment env =
  match non_base_codegen_variant env with
  | None -> env
  | Some name ->
    let keys = Codegen_variant_name.ocamlparams (variant_of_name name) in
    let current =
      if Environments.is_variable_defined Ocaml_variables.ocamlparam env
      then Environments.safe_lookup Ocaml_variables.ocamlparam env
      else Ocamltest_stdlib.Sys.safe_getenv "OCAMLPARAM"
    in
    let value =
      if String.equal current ""
      then String.concat "," (keys @ ["_"])
      else
        (* A value may start with the separator that it uses. *)
        match current.[0] with
        | (':' | '|' | ';' | ' ' | ',') as separator ->
          let separator = String.make 1 separator in
          separator ^ String.concat separator keys ^ current
        | _ -> String.concat "," keys ^ "," ^ current
    in
    with_variant_c_flags
      (Environments.add Ocaml_variables.ocamlparam value env)

(* What a command of this kind does with the variant is not visible from
   here, so it counts as work that depends on it. *)
let run_for_variant run log env =
  let task, env_after = run ~command_env:command_environment log env in
  variant_work_for env task, env_after

let include_toplevel_directory =
  "-I " ^ Ocaml_directories.toplevel

let c_includes =
  let dir = Ocaml_directories.runtime in
  "-ccopt -I" ^ dir

let runtime_variant_flags () = match Ocaml_files.runtime_variant() with
  | Ocaml_files.Normal -> ""
  | Ocaml_files.Debug -> " -runtime-variant d"
  | Ocaml_files.Instrumented -> " -runtime-variant i"

let target_flags = function
  | Ocaml_backends.Native | Ocaml_backends.Bytecode -> ""
  | Ocaml_backends.Jsir_js -> "-target js_of_ocaml"
  | Ocaml_backends.Jsir_wasm -> "-target wasm_of_ocaml"

let runtime_flags env backend c_files =
  let runtime_library_flags = "-I " ^
    Ocaml_directories.runtime in
  let rt_flags = match backend with
    | Ocaml_backends.Native -> runtime_variant_flags ()
    | Ocaml_backends.Jsir_js | Ocaml_backends.Jsir_wasm -> ""
    | Ocaml_backends.Bytecode ->
      begin
        if c_files then begin (* custom mode *)
          "-custom " ^ (runtime_variant_flags ())
        end else begin (* non-custom mode *)
          let use_runtime =
            Environments.lookup_as_bool Ocaml_variables.use_runtime env
          in
          if use_runtime = Some false
          then ""
          else "-use-runtime " ^ Ocaml_files.ocamlrun
        end
      end in
  rt_flags ^ " " ^ runtime_library_flags

let toplevel_default_flags =
  "-noinit -no-version -noprompt \
   -alert -unsafe_multidomain \
   -alert -do_not_spawn_domains \
   -alert -unsafe_effects"

let ocamlobjinfo_default_flags = "-null-crc"
