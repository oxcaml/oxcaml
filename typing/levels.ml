open Local_store

(**** Type level management ****)

let current_level = s_ref 0

let nongen_level = s_ref 0

let global_level = s_ref 0

let saved_level = s_ref []

let get_current_level () = !current_level

let get_global_level () = !global_level

let get_nongen_level () = !nongen_level

let init_def level =
  current_level := level;
  nongen_level := level

let begin_def () =
  saved_level := (!current_level, !nongen_level) :: !saved_level;
  incr current_level;
  nongen_level := !current_level

let begin_class_def () =
  saved_level := (!current_level, !nongen_level) :: !saved_level;
  incr current_level

let raise_nongen_level () =
  saved_level := (!current_level, !nongen_level) :: !saved_level;
  nongen_level := !current_level

let end_def () =
  let cl, nl = List.hd !saved_level in
  saved_level := List.tl !saved_level;
  current_level := cl;
  nongen_level := nl

let create_scope () =
  init_def (!current_level + 1);
  !current_level

let wrap_end_def f = Misc.try_finally f ~always:end_def

let with_local_level ?post f =
  begin_def ();
  let result = wrap_end_def f in
  Option.iter (fun g -> g result) post;
  result

let with_local_level_if cond f ~post =
  if cond then with_local_level f ~post else f ()

let with_local_level_iter f ~post =
  begin_def ();
  let result, l = wrap_end_def f in
  List.iter post l;
  result

let with_local_level_iter_if cond f ~post =
  if cond then with_local_level_iter f ~post else fst (f ())

let with_local_level_if_principal f ~post =
  with_local_level_if !Clflags.principal f ~post

let with_local_level_iter_if_principal f ~post =
  with_local_level_iter_if !Clflags.principal f ~post

let with_level ~level f =
  begin_def ();
  init_def level;
  let result = wrap_end_def f in
  result

let with_level_if cond ~level f = if cond then with_level ~level f else f ()

let with_local_level_for_class ?post f =
  begin_class_def ();
  let result = wrap_end_def f in
  Option.iter (fun g -> g result) post;
  result

let with_raised_nongen_level f =
  raise_nongen_level ();
  wrap_end_def f

let reset_global_level () = global_level := !current_level

let increase_global_level () =
  let gl = !global_level in
  global_level := !current_level;
  gl

let restore_global_level gl = global_level := gl

let update_current_level l = current_level := l

(* For use with ocamldebug *)
type global_state =
  { current_level : int ref;
    nongen_level : int ref;
    global_level : int ref
  }

let global_state : global_state = { current_level; nongen_level; global_level }

let print_global_state fmt global_state =
  let print_field fmt s r = Format.fprintf fmt "%s = %d;@;" s !r in
  let print_fields fmt { current_level; nongen_level; global_level } =
    print_field fmt "current_level" current_level;
    print_field fmt "nongen_level" nongen_level;
    print_field fmt "global_level" global_level
  in
  Format.fprintf fmt "@[<1>{@;%a}@]" print_fields global_state
