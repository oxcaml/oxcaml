(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Jules Jacobs, Jane Street                             *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Vox_logic

module Config = struct
  type t =
    { timeout_seconds : float option
    ; z3_command : string option
    }

  let default = { timeout_seconds = Some 10.; z3_command = None }
end

type reason =
  | Timeout
  | Incomplete of string

type model = (string * Term.t) list

type verdict =
  | Proved of { unused_hypotheses : int list option }
  | Refuted of model option
  | Unknown of reason

type failure =
  | Unavailable of string
  | Error of
      { cause : string
      ; raw : string
      }

type outcome = (verdict, failure) result

module type BACKEND = sig
  val name : string
  val configured : config:Config.t -> (unit, string) result
  val discharge : config:Config.t -> Obligation.t -> outcome
end

let solver_error ~cause ~raw : outcome = Result.Error (Error { cause; raw })

let ill_formed message : outcome =
  solver_error ~cause:"ill-formed obligation" ~raw:message

let timeout_ms (config : Config.t) =
  Option.map
    (fun seconds -> max 1 (int_of_float (Float.round (seconds *. 1000.))))
    config.timeout_seconds

(* Running a command.  [Sys.command] is all we have without the unix
   library; output is captured through a temporary file, which honours
   [TMPDIR]. *)

let run_command command =
  let output_file = Filename.temp_file "vox_solver" ".out" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove output_file with Sys_error _ -> ())
    (fun () ->
       let code =
         Sys.command (command ^ " > " ^ Filename.quote output_file ^ " 2>&1")
       in
       let contents =
         let channel = open_in_bin output_file in
         Fun.protect
           ~finally:(fun () -> close_in channel)
           (fun () -> really_input_string channel (in_channel_length channel))
       in
       code, contents)

let with_script_file script f =
  let script_file = Filename.temp_file "vox_query" ".smt2" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove script_file with Sys_error _ -> ())
    (fun () ->
       let channel = open_out_bin script_file in
       Fun.protect
         ~finally:(fun () -> close_out channel)
         (fun () -> output_string channel script);
       f script_file)

module Printing : BACKEND = struct
  let name = "printing"

  let configured ~config:(_ : Config.t) = Ok ()

  let discharge ~config obligation =
    match
      Vox_smtlib.render ?timeout_ms:(timeout_ms config) Prove obligation
    with
    | Result.Error message -> ill_formed message
    | Ok script ->
      (* Via [std_formatter], like other compiler output (and so the expect
         harness, which captures formatters rather than file descriptors,
         sees it). *)
      Format.printf "%s@?" script;
      Ok (Unknown (Incomplete "printing backend discharges nothing"))
end

(* What z3 said, structurally.  z3 keeps going after an inapplicable
   directive ([get-model] after [unsat], ...), printing an [(error ...)]
   line for it and exiting nonzero, so the exit code means nothing once a
   status line is present and error entries are skipped by the readers
   below. *)
module Solver_output : sig
  type sexp =
    | Atom of string
    | List of sexp list

  type t =
    { status : string  (** "sat", "unsat" or "unknown" *)
    ; sexps : sexp list  (** everything after the status line *)
    ; rejected : bool
          (** an [(error ...)] appeared {e before} the status line: the
              solver rejected part of the script, so the status answers a
              different question and must not become a verdict.  Errors
              after the status are the inapplicable directives and stay
              ignorable. *)
    }

  val parse : string -> t option

  (** The ids named in an unsat core [(h0 h2 ...)]; [None] when no core is
      present. *)
  val core_ids : t -> int list option

  (** The values of [variables] in a [(model ...)], read back as terms.
      Best-effort: an atom that is neither a literal nor a nullary
      constructor of [signature] reads back as a {!Term.Var} (z3 prints
      opaque universe elements like [t!val!0] for uninterpreted sorts), and
      a variable whose value does not read back as a term is dropped. *)
  val model : signature:Signature.t -> t -> model option

  (** Whether [(get-info :reason-unknown)] answered "timeout" or
      "canceled". *)
  val timed_out : t -> bool
end = struct
  type sexp =
    | Atom of string
    | List of sexp list

  type t =
    { status : string
    ; sexps : sexp list
    ; rejected : bool
        (* an [(error ...)] appeared before the status line: the solver
           rejected part of the script and the status answers a different
           question.  Errors after the status are the inapplicable
           directives ([get-model] after [unsat], ...) and stay ignorable. *)
    }

  (* A lexer for the sexps z3 prints: parentheses, atoms, [|...|] quoted
     symbols, ["..."] strings (their delimiters are dropped), and [;]
     comments (z3 writes commentary inside models). *)
  let tokenize text =
    let tokens = ref [] in
    let i = ref 0 in
    let n = String.length text in
    while !i < n do
      (match text.[!i] with
       | ' ' | '\t' | '\n' | '\r' -> incr i
       | ';' -> while !i < n && text.[!i] <> '\n' do incr i done
       | '(' -> tokens := "(" :: !tokens; incr i
       | ')' -> tokens := ")" :: !tokens; incr i
       | '|' | '"' ->
         let close = text.[!i] in
         let start = !i + 1 in
         let stop = ref start in
         while !stop < n && text.[!stop] <> close do incr stop done;
         tokens := String.sub text start (!stop - start) :: !tokens;
         i := min n (!stop + 1)
       | _ ->
         let start = !i in
         let delimiter = function
           | ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '|' | '"' -> true
           | _ -> false
         in
         while !i < n && not (delimiter text.[!i]) do incr i done;
         tokens := String.sub text start (!i - start) :: !tokens)
    done;
    List.rev !tokens

  (* Parse as many complete sexps as the tokens form; an unfinished tail is
     dropped rather than an error, since the readers below are all
     best-effort. *)
  let parse_sexps tokens =
    let rec one = function
      | [] -> None
      | "(" :: rest ->
        let items, rest = many rest in
        (match rest with
         | ")" :: rest -> Some (List items, rest)
         | _ -> None)
      | ")" :: _ -> None
      | atom :: rest -> Some (Atom atom, rest)
    and many tokens =
      match one tokens with
      | Some (sexp, rest) ->
        let items, rest = many rest in
        sexp :: items, rest
      | None -> [], tokens
    in
    fst (many tokens)

  let parse text =
    let lines = String.split_on_char '\n' text in
    let rec split_at_status before = function
      | [] -> None
      | line :: rest ->
        (match String.trim line with
         | ("sat" | "unsat" | "unknown") as status ->
           Some (List.rev before, status, rest)
         | _ -> split_at_status (line :: before) rest)
    in
    match split_at_status [] lines with
    | None -> None
    | Some (before, status, rest) ->
      let rejected =
        List.exists
          (fun line ->
             String.length (String.trim line) >= 6
             && String.equal (String.sub (String.trim line) 0 6) "(error")
          before
      in
      Some
        { status
        ; sexps = parse_sexps (tokenize (String.concat "\n" rest))
        ; rejected
        }

  let hypothesis_id = function
    | List _ -> None
    | Atom atom ->
      if String.length atom >= 2
         && atom.[0] = 'h'
         && String.for_all
              (function '0' .. '9' -> true | _ -> false)
              (String.sub atom 1 (String.length atom - 1))
      then int_of_string_opt (String.sub atom 1 (String.length atom - 1))
      else None

  let core_ids t =
    List.find_map
      (function
        | Atom _ -> None
        | List items ->
          let ids = List.filter_map hypothesis_id items in
          if List.length ids = List.length items then Some ids else None)
      t.sexps

  exception Not_a_term

  let bits_value ~bits_per_char ~char_value digits =
    let width = String.length digits * bits_per_char in
    if width < 1 || width > 64 then raise Not_a_term;
    let value =
      String.fold_left
        (fun acc c ->
           Int64.logor
             (Int64.shift_left acc bits_per_char)
             (Int64.of_int (char_value c)))
        0L digits
    in
    Literal.Bitvec { width; value }

  let rec term_of_sexp constructors sexp : Term.t =
    match sexp with
    | Atom "true" -> Const (Bool true)
    | Atom "false" -> Const (Bool false)
    | Atom atom
      when atom <> ""
           && String.for_all (function '0' .. '9' -> true | _ -> false) atom
      ->
      Const (Int atom)
    | Atom atom when String.length atom > 2 && atom.[0] = '#' ->
      let digits = String.sub atom 2 (String.length atom - 2) in
      (match atom.[1] with
       | 'b' ->
         Const
           (bits_value ~bits_per_char:1
              ~char_value:(function
                | '0' -> 0 | '1' -> 1 | _ -> raise Not_a_term)
              digits)
       | 'x' ->
         Const
           (bits_value ~bits_per_char:4
              ~char_value:(function
                | '0' .. '9' as c -> Char.code c - Char.code '0'
                | 'a' .. 'f' as c -> Char.code c - Char.code 'a' + 10
                | 'A' .. 'F' as c -> Char.code c - Char.code 'A' + 10
                | _ -> raise Not_a_term)
              digits)
       | _ -> raise Not_a_term)
    | Atom atom when Hashtbl.mem constructors atom -> Construct (atom, [])
    | Atom atom -> Var atom
    | List [Atom "-"; magnitude] ->
      (match term_of_sexp constructors magnitude with
       | Const (Int digits) -> Const (Int ("-" ^ digits))
       | _ -> raise Not_a_term)
    | List (Atom constructor :: arguments)
      when Hashtbl.mem constructors constructor ->
      Construct
        (constructor, List.map (term_of_sexp constructors) arguments)
    | List _ -> raise Not_a_term

  let model ~(signature : Signature.t) t =
    let constructors = Hashtbl.create 16 in
    List.iter
      (fun (datatype : Signature.datatype) ->
         List.iter
           (fun (constructor : Signature.constructor) ->
              Hashtbl.replace constructors constructor.constructor_name ())
           datatype.constructors)
      signature.datatypes;
    let definitions =
      List.find_map
        (function
          | Atom _ -> None
          | List (Atom "model" :: definitions) -> Some definitions
          | List definitions
            when List.exists
                   (function
                     | List (Atom "define-fun" :: _) -> true
                     | _ -> false)
                   definitions ->
            Some definitions
          | List _ -> None)
        t.sexps
    in
    Option.map
      (fun definitions ->
         List.filter_map
           (function
             | List [Atom "define-fun"; Atom name; List []; _sort; value]
               when List.mem_assoc name signature.variables ->
               (match term_of_sexp constructors value with
                | term -> Some (name, term)
                | exception Not_a_term -> None)
             | _ -> None)
           definitions)
      definitions

  let timed_out t =
    List.exists
      (function
        | List [Atom ":reason-unknown"; Atom ("timeout" | "canceled")] -> true
        | _ -> false)
      t.sexps
end

module Z3 : BACKEND = struct
  let name = "z3"

  let not_configured =
    "no solver configured: give the z3 backend a command, or pass \
     -vox-backend none to typecheck only"

  let configured ~(config : Config.t) =
    match config.z3_command with
    | None -> Result.Error not_configured
    | Some command ->
      (match run_command (command ^ " -version") with
       | 0, _ -> Ok ()
       | code, output ->
         Result.Error
           (Printf.sprintf "solver command %s failed (exit code %d): %s"
              command code (String.trim output)))

  (* The z3-side [:timeout] is asked for in the script; the wall-clock kill
     backs it up, with a grace second, because a wedged process ignores
     options. *)
  let run (config : Config.t) command script =
    let wrapped_command =
      match config.timeout_seconds with
      | None -> command
      | Some seconds -> Printf.sprintf "timeout %.3f %s" (seconds +. 1.) command
    in
    with_script_file script (fun script_file ->
      let code, output =
        run_command (wrapped_command ^ " " ^ Filename.quote script_file)
      in
      if code = 124
      then Ok None (* killed by the wall clock *)
      else
        match Solver_output.parse output with
        | Some parsed when parsed.rejected ->
          Result.Error
            (Error { cause = "the solver rejected the query"; raw = output })
        | Some parsed ->
          if String.equal parsed.status "unknown"
             && Solver_output.timed_out parsed
          then Ok None
          else Ok (Some parsed)
        | None ->
          Result.Error
            (Error
               { cause =
                   Printf.sprintf "the solver gave no answer (exit code %d)"
                     code
               ; raw = output
               }))

  let discharge ~(config : Config.t) (obligation : Obligation.t) =
    match config.z3_command with
    | None -> Result.Error (Unavailable not_configured)
    | Some command ->
      let render query =
        Vox_smtlib.render ?timeout_ms:(timeout_ms config) query obligation
      in
      let run_query script k =
        match run config command script with
        | Result.Error failure -> Result.Error failure
        | Ok None -> Ok (Unknown Timeout)
        | Ok (Some (output : Solver_output.t)) -> k output
      in
      (match render Prove with
       | Result.Error message -> ill_formed message
       | Ok prove_script ->
         run_query prove_script (fun prove ->
           match prove.status with
           | "unsat" ->
             let unused_hypotheses =
               Option.map
                 (fun used ->
                    List.filter_map
                      (fun (hypothesis : Obligation.hypothesis) ->
                         if List.mem hypothesis.id used
                         then None
                         else Some hypothesis.id)
                      obligation.hypotheses)
                 (Solver_output.core_ids prove)
             in
             Ok (Proved { unused_hypotheses })
           | "sat" ->
             (match render Disprove with
              | Result.Error message -> ill_formed message
              | Ok disprove_script ->
                run_query disprove_script (fun disprove ->
                  match disprove.status with
                  | "unsat" ->
                    Ok
                      (Refuted
                         (Solver_output.model ~signature:obligation.signature
                            prove))
                  | _ ->
                    Ok
                      (Unknown
                         (Incomplete
                            (Printf.sprintf
                               "prove query: %s; disprove query: %s"
                               prove.status disprove.status)))))
           | _ ->
             (* "unknown", and not by timeout.  The disprove query is not
                run: hypothesis satisfiability was not established, and
                contradictory hypotheses make [hyps AND goal] unsat while
                the correct verdict is Proved, so a refutation cannot be
                claimed. *)
             Ok
               (Unknown
                  (Incomplete
                     (Printf.sprintf "prove query: %s" prove.status)))))
end

let backends : (module BACKEND) list = [(module Printing); (module Z3)]

let backend_names =
  List.map (fun (module Backend : BACKEND) -> Backend.name) backends

let select name =
  match
    List.find_opt
      (fun (module Backend : BACKEND) -> String.equal Backend.name name)
      backends
  with
  | Some backend -> Ok backend
  | None ->
    Result.Error
      (Printf.sprintf
         "unknown vox backend %s (valid backends: %s; or none to typecheck \
          only)"
         name
         (String.concat ", " backend_names))

type plan =
  | No_discharge
  | Discharge of (module BACKEND)

let plan ~backend_name ~config =
  if String.equal backend_name "none"
  then Ok No_discharge
  else
    match select backend_name with
    | Result.Error message -> Result.Error message
    | Ok ((module Backend : BACKEND) as backend) ->
      (match Backend.configured ~config with
       | Ok () -> Ok (Discharge backend)
       | Result.Error message ->
         Result.Error (Printf.sprintf "%s backend: %s" Backend.name message))
