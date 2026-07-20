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

module Quadrant = struct
  type t =
    | Agree_noalloc (* FE accept, BE pass *)
    | Agree_alloc (* FE reject, BE reject *)
    | Soundness_suspect (* FE accept, BE reject -- ambiguous, triage *)
    | Precision_gap (* FE reject, BE pass -- trustworthy frontend gap *)
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

(* Low optimization level on purpose: keeps BE-pass close to the naive lowering
   so precision gaps are attributable to fixable frontend conservatism. *)
let backend_flags =
  [ "-c";
    "-O0";
    "-zero-alloc-check";
    "default";
    "-color";
    "never";
    "-error-style";
    "short" ]

let run_frontend ~compiler ~file =
  let code, text = run_compiler compiler (frontend_flags @ [file]) in
  (* CR shsong: a syntactically bad body would also give nonzero here; such
     cases are caught by the backend returning [Gen_error] and are dropped in
     [classify]. Refine to inspect [text] for the mode-error signature if
     needed. *)
  if code = 0 then Verdict.Accept else Verdict.Reject text

let run_backend ~compiler ~file =
  let code, text = run_compiler compiler (backend_flags @ [file]) in
  match code with
  | 0 -> Verdict.Accept
  | 2 -> Verdict.Reject text
  | _ -> Verdict.Gen_error text

let classify ~frontend ~backend =
  match frontend, backend with
  | Verdict.Gen_error e, _ | _, Verdict.Gen_error e -> Error e
  | Verdict.Accept, Verdict.Accept -> Ok Quadrant.Agree_noalloc
  | Verdict.Reject _, Verdict.Reject _ -> Ok Quadrant.Agree_alloc
  | Verdict.Accept, Verdict.Reject _ -> Ok Quadrant.Soundness_suspect
  | Verdict.Reject _, Verdict.Accept -> Ok Quadrant.Precision_gap
