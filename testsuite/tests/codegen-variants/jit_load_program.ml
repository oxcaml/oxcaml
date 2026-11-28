(* TEST
 flags = "-I +compiler-libs -I +unix -I +dynlink -I +ocaml-jit -linkall";
 libraries = "unix dynlink ocamlcommon ocamlfrontend ocamloptcomp";
 libraries += " ocamlopttoplevel jit";
 not-macos;
 native;
*)

(* Jit.jit_load_program compiles a program with the current flags and runs
   it in the image of its caller, so it refuses flags that select other
   frame-pointer or stack-check settings than its caller was built with (or
   another AddressSanitizer setting, which this test leaves alone). The native
   toplevel and eval check this themselves before they reach it; this
   program calls it directly, as eval does, without checking anything. The
   check is the one of the native toplevel, and it reads the flags that the
   backend reads: a target for which they select no variant does not hide
   them, and the refusal names the variant that they select. With the flags
   of the caller, such a target is refused for itself: the program would be
   compiled to native code as the middle end prepares it for that target.
   So is the bytecode target, which is the target of a program that sets
   none: the middle end prepares nothing for native code under it.

   Poll insertion is not one of those settings. Jit.jit_load_program follows
   the rule for code that a program loads into itself: code with polls loads
   into any program, and code without polls into any program but a fully
   polled one that may run more than one domain. The flags of this program
   have polls exactly when it is fully polled, so the last load below, with
   the other setting, is refused for its polls in such a program and
   accepted in every other.

   The JIT takes the code of the native backend as it is emitted, so no
   program can load the code of the LLVM backend through it. A toplevel
   that has registered the JIT says so for a phrase, before it asks about
   the polls of the phrase: what it would say of those (compile with poll
   insertion, or run one domain) does not make the phrase loadable.

   Not run on macOS, like installed_jit.ml. *)

let program_of_source ~input_name source =
  let lexbuf = Lexing.from_string source in
  Location.input_lexbuf := Some lexbuf;
  Location.init lexbuf "//jit_load_program//";
  let ast = Parse.implementation lexbuf in
  let compilation_unit =
    Compilation_unit.create Compilation_unit.Prefix.empty
      (Compilation_unit.Name.of_string input_name)
  in
  let unit_info = Unit_info.make_dummy ~input_name compilation_unit in
  Compilenv.reset unit_info;
  let env = Compmisc.initial_env () in
  let typed_impl =
    Typemod.type_implementation unit_info compilation_unit env ast
  in
  let tlambda_program =
    Translmod.transl_implementation compilation_unit
      ~loc:(Location.curr lexbuf)
      ( typed_impl.structure,
        typed_impl.coercion,
        Option.map
          (fun (ai : Typedtree.argument_interface) ->
            ai.ai_coercion_from_primary)
          typed_impl.argument_interface )
  in
  let lambda =
    let _static_data, raw_lambda =
      Slambda.eval ~cu_static_data:Compilenv.get_static_data
        tlambda_program.code
    in
    Simplif.simplify_lambda
      ~restrict_to_upstream_dwarf:!Clflags.restrict_to_upstream_dwarf
      ~gdwarf_may_alter_codegen:!Dwarf_flags.gdwarf_may_alter_codegen
      raw_lambda
  in
  { tlambda_program with code = lambda }

external max_domain_count : unit -> int = "caml_max_domain_count"
[@@noalloc]

external code_is_fully_polled : unit -> bool = "caml_code_is_fully_polled"
[@@noalloc]

type outcome =
  | Loaded
  | Refused_for_its_variant of string
  | Refused_for_its_target
  | Refused_for_its_polls
  | Failed of string

let load ~input_name program =
  match
    Jit.jit_load_program ~phrase_name:input_name Format.err_formatter program
  with
  | Result _ -> Loaded
  | Exception exn -> Failed ("the program raised " ^ Printexc.to_string exn)
  | exception Failure message ->
    let starts_with prefix = String.starts_with ~prefix message in
    if starts_with "this executable was built for codegen variant"
    then Refused_for_its_variant message
    else if starts_with "this executable compiles phrases to native code"
    then Refused_for_its_target
    else if starts_with
              "in-process loading of code compiled without poll insertion"
    then Refused_for_its_polls
    else Failed message

let contains ~part message =
  let rec search i =
    i + String.length part <= String.length message
    && (String.equal (String.sub message i (String.length part)) part
       || search (i + 1))
  in
  search 0

(* The variant that a refusal says the phrases are for is the one that the
   flags select at the time, whatever the target. *)
let names_the_variant_of_the_flags message =
  contains message
    ~part:
      (Printf.sprintf "phrases for variant '%s' into"
         (Codegen_variant.to_string (Codegen_variant.flags_variant ())))

let show description outcome =
  Printf.printf "%s: %s\n%!" description
    (match outcome with
    | Loaded -> "loaded"
    | Refused_for_its_variant message ->
      if names_the_variant_of_the_flags message
      then "refused"
      else "refused, but not as the variant of the flags: " ^ message
    | Refused_for_its_target -> "refused for its target"
    | Refused_for_its_polls -> "refused for its polls"
    | Failed message -> message)

let with_flipped flag f =
  flag := not !flag;
  f ();
  flag := not !flag

let () =
  Clflags.set_target Clflags.Target.Native;
  Clflags.Opt_flag_handler.set Oxcaml_flags.opt_flag_handler;
  Clflags.dont_write_files := true;
  Clflags.shared := true;
  Clflags.dlcode := false;
  Compmisc.init_path ();
  let input_name = "Jit_load_program___0" in
  let program = program_of_source ~input_name "let answer = 6 * 7" in
  let attempt description = show description (load ~input_name program) in
  with_flipped Clflags.frame_pointers (fun () ->
      attempt "frame pointers unlike the caller");
  with_flipped Clflags.stack_checks (fun () ->
      attempt "stack checks unlike the caller");
  (* What is compiled here is native code whatever the target says. *)
  Clflags.set_target Clflags.Target.Js_of_ocaml;
  with_flipped Clflags.frame_pointers (fun () ->
      attempt "frame pointers unlike the caller, with another target");
  attempt "the flags of the caller, with another target";
  (* The target of a program that sets none. *)
  Clflags.set_target Clflags.Target.Bytecode;
  attempt "the flags of the caller, with no target set";
  Clflags.set_target Clflags.Target.Native;
  attempt "the flags of the caller";
  let input_name = "Jit_load_program___1" in
  let program = program_of_source ~input_name "let answer = 6 * 9" in
  with_flipped Clflags.poll_insertion (fun () ->
      let needs_polls = code_is_fully_polled () && max_domain_count () > 1 in
      match load ~input_name program, needs_polls with
      | Loaded, false | Refused_for_its_polls, true ->
        print_endline
          "poll insertion unlike the caller: as the rule for loaded code says"
      | outcome, _ -> show "poll insertion unlike the caller" outcome);
  (* A toplevel that has registered the JIT gives it its phrases. *)
  Jit.init_top ();
  Opttoploop.initialize_toplevel_env ();
  let phrase =
    !Opttoploop.parse_toplevel_phrase (Lexing.from_string "6 * 7;;")
  in
  Clflags.llvm_backend := true;
  with_flipped Clflags.poll_insertion (fun () ->
      match Opttoploop.execute_phrase false Format.std_formatter phrase with
      | (_ : bool) -> print_endline "a phrase for the LLVM backend: executed"
      | exception Failure message ->
        Printf.printf "a phrase for the LLVM backend: %s\n" message)
