module Verdict = struct
  type t =
    | Accept (* the check succeeded (exit 0) *)
    | Reject of
        string (* the check failed as expected; payload = captured stderr *)
    | Gen_error of string (* compile failed for an unrelated reason *)

  (* Helper to extract canonical "cause" of a frontend rejection, for
     deduplicating precision gaps into a ranked backlog. For now: the first line
     beginning with "Error:".

     CR shsong: refine to normalize locations / extract the offending value
     name. *)
  let first_error_line msg =
    let is_error l = String.length l >= 6 && String.sub l 0 6 = "Error:" in
    let rec find = function
      | [] -> "unknown"
      | l :: rest ->
        let l = String.trim l in
        if is_error l then l else find rest
    in
    find (String.split_on_char '\n' msg)

  let cause v =
    match v with
    | Reject msg -> first_error_line msg
    | Accept | Gen_error _ -> "unknown"
end

module Outcome = struct
  type gate_stage =
    | Fe_gate
    | Be_gate

  type t =
    | Agree_noalloc (* FE accepts, BE passes -- agree *)
    | Soundness_suspect of { backend_error : string }
      (* FE accepts, BE rejects -- ambiguous, triage *)
    | Fe_reject of { cause : string }
      (* FE rejects -- possible precision gap; not confirmable by the backend *)
    | Prelude_reject of
        { stage : gate_stage;
          cause : string
        }
  (* the prelude alone failed its own FE or BE check *)
end

(* Run [compiler] with [args], returning (exit_code, captured_stderr). stdout is
   discarded; the checks we care about report on stderr. Uses [Sys.command] (via
   /bin/sh) rather than the [unix] library so the tool builds with the stdlib
   alone -- the boot compiler used for the LSP cannot build [unix] from this
   repo's sources. [compiler] and every arg are shell-quoted, so an absolute
   [compiler] path is required (a bare name would be resolved by the shell's
   PATH). *)
let run_compiler compiler args =
  let stderr_file = Filename.temp_file "qc_alloc" ".stderr" in
  let cmd =
    Printf.sprintf "%s %s > /dev/null 2> %s" (Filename.quote compiler)
      (String.concat " " (List.map Filename.quote args))
      (Filename.quote stderr_file)
  in
  (* [Sys.command] returns the shell's exit status, i.e. the compiler's exit
     code (or 128 + signal if it was killed). *)
  let code = Sys.command cmd in
  let ic = open_in_bin stderr_file in
  let text = really_input_string ic (in_channel_length ic) in
  close_in ic;
  (try Sys.remove stderr_file with _ -> ());
  code, text

let frontend_flags =
  [ "-c";
    "-stop-after";
    "typing";
    "-extension";
    "mode_alpha";
    "-color";
    "never";
    "-error-style";
    "short" ]

(* No -O flag: the default is the lowest optimization level (this compiler has
   no -O0/-O1; only -O2/-O3 raise it). Keeping it low keeps BE-pass close to the
   naive lowering, so precision gaps are attributable to fixable frontend
   conservatism rather than to allocations flambda would optimize away.

   The mode extension is needed here too: the backend run compiles the same
   doubly-annotated source, which contains mode syntax. *)
let backend_flags =
  [ "-c";
    "-zero-alloc-check";
    "default";
    "-extension";
    "mode_alpha";
    "-color";
    "never";
    "-error-style";
    "short" ]

(* Does [text] contain [needle]? Substring search, stdlib only. *)
let contains ~needle text =
  let n = String.length text and k = String.length needle in
  let rec loop i =
    if i + k > n
    then false
    else if String.sub text i k = needle
    then true
    else loop (i + 1)
  in
  loop 0

(* CR shsong: Frontend should distinguish allocation error from other error such
   as local/global errors. *)
let run_frontend ~compiler ~file =
  let code, text = run_compiler compiler (frontend_flags @ [file]) in
  (* CR shsong: a syntactically bad body would also give nonzero here; such
     cases are caught by the backend returning [Gen_error] and are dropped in
     [classify]. Refine to inspect [text] for the mode-error signature if
     needed. *)
  if code = 0 then Verdict.Accept else Verdict.Reject text

let run_backend ~compiler ~file =
  let code, text = run_compiler compiler (backend_flags @ [file]) in
  (* Exit 2 is OCaml's generic error code, not only a zero_alloc failure (e.g.
     an unknown flag or a malformed body also exits 2). A genuine rejection must
     carry the zero_alloc checker's signature; anything else is an unrelated
     compile failure, i.e. a generator bug. *)
  if code = 0
  then Verdict.Accept
  else if code = 2 && contains ~needle:"Annotation check for zero_alloc" text
  then Verdict.Reject text
  else Verdict.Gen_error text

(* The gate: the prelude must pass both checks on its own before the main
   function's verdicts mean anything (a checked prelude annotation that is
   inconsistent with its body would otherwise masquerade as a main-function
   result). *)
let check_prelude ~compiler ~prelude_file =
  match run_frontend ~compiler ~file:prelude_file with
  | Verdict.Gen_error e -> Error (`Gen_error e)
  | Verdict.Reject _ as v -> Error (`Reject (Outcome.Fe_gate, Verdict.cause v))
  | Verdict.Accept -> (
    match run_backend ~compiler ~file:prelude_file with
    | Verdict.Gen_error e -> Error (`Gen_error e)
    | Verdict.Reject _ as v ->
      Error (`Reject (Outcome.Be_gate, Verdict.cause v))
    | Verdict.Accept -> Ok ())

let check ~compiler ~prelude_file ~file =
  match check_prelude ~compiler ~prelude_file with
  | Error (`Gen_error e) -> Error e
  | Error (`Reject (stage, cause)) ->
    Ok (Outcome.Prelude_reject { stage; cause })
  | Ok () -> (
    match run_frontend ~compiler ~file with
    | Verdict.Gen_error e -> Error e
    | Verdict.Reject _ as frontend ->
      (* The program does not typecheck, so the backend check cannot run on
         it. *)
      Ok (Outcome.Fe_reject { cause = Verdict.cause frontend })
    | Verdict.Accept -> (
      match run_backend ~compiler ~file with
      | Verdict.Gen_error e -> Error e
      | Verdict.Accept -> Ok Outcome.Agree_noalloc
      | Verdict.Reject backend_error ->
        Ok (Outcome.Soundness_suspect { backend_error })))
