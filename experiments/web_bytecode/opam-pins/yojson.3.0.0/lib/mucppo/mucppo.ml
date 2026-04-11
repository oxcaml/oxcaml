(* mucppo Copyright 2023-2024 Marek Kubica <marek@tarides.com>
 * Released under CC0 license, freely available to all.
 *
 * Simple, no dependency cppo replacement to be embedded into builds.
 *
 * Contains a bare subset of cppo features to eliminate it as a dependency.
 * For more info check the project page at
 * https://github.com/Leonidas-from-XIV/mucppo
 *)

let version_triple major minor patch = (major, minor, patch)
let current_version = Scanf.sscanf Sys.ocaml_version "%u.%u.%u" version_triple
let greater_or_equal (v : int * int * int) = current_version >= v

module PrintingState : sig
  type t

  val empty : t
  val is_empty : t -> bool
  val flip_top : t -> t
  val latest_was_triggered : t -> bool
  val pop : t -> t
  val push : bool -> t -> t
  val should_print : t -> bool
end = struct
  type state = { state : bool; was_true : bool }
  type t = state list

  let empty = [ { state = true; was_true = false } ]
  let is_empty (x : t) = x = empty

  let flip_top = function
    | [] -> failwith "Output stack empty, invalid state"
    | x :: xs -> { state = not x.state; was_true = true } :: xs

  let latest_was_triggered l = (List.hd l).was_true
  let pop = List.tl
  let push state l = { state; was_true = state } :: l

  let should_print =
    List.fold_left (fun acc { state; was_true = _ } -> state && acc) true
end

module Variables = struct
  module Map = Map.Make (String)

  let is_defined name = Map.mem name
  let define name = Map.add name ()
  let undefine name = Map.remove name
  let empty = Map.empty
end

(* for OCaml 4.02 *)
let string_equal = (=)

let starts_with ~prefix s =
  let len = String.length prefix in
  String.length s >= len && string_equal (String.sub s 0 len) prefix

let is_if_statement = starts_with ~prefix:"#if"
let is_elif_defined_statement = starts_with ~prefix:"#elif defined"
let is_include_statement = starts_with ~prefix:"#include"
let is_define_statement = starts_with ~prefix:"#define"
let is_undef_statement = starts_with ~prefix:"#undef"
let is_ifdef = starts_with ~prefix:"#ifdef"
let filename_of_include s = Scanf.sscanf s "#include %S" (fun x -> x)
let variable_of_define s = Scanf.sscanf s "#define %s" (fun x -> x)
let variable_of_undef s = Scanf.sscanf s "#undef %s" (fun x -> x)
let variable_of_ifdef s = Scanf.sscanf s "#ifdef %s" (fun x -> x)
let variable_of_elif_defined s = Scanf.sscanf s "#elif defined %s" (fun x -> x)

let is_ocaml_version s =
  (* Sscanf.sscanf_opt exists but only since 5.0 *)
  match Scanf.sscanf s "#if OCAML_VERSION >= (%u, %u, %u)" version_triple with
  | v -> Some v
  | exception _ -> None

module State = struct
  type t = {
    (* print state *)
    ps : PrintingState.t;
    vars : unit Variables.Map.t;
  }

  let conditional_triggered s = { s with ps = PrintingState.flip_top s.ps }
  let end_conditional s = { s with ps = PrintingState.pop s.ps }
  let triggered_before s = PrintingState.latest_was_triggered s.ps
  let start_conditional v s = { s with ps = PrintingState.push v s.ps }
  let should_output { ps; vars = _ } = PrintingState.should_print ps

  let don't_print s =
    match should_output s with
    | false -> s
    | true -> { s with ps = PrintingState.flip_top s.ps }

  let finished { ps; vars = _ } = PrintingState.is_empty ps
  let empty = { ps = PrintingState.empty; vars = Variables.empty }
  let define v s = { s with vars = Variables.define v s.vars }
  let undefine v s = { s with vars = Variables.undefine v s.vars }
  let is_defined v { ps = _; vars } = Variables.is_defined v vars
end

let output_endline oc s =
  output_string oc s;
  output_char oc '\n'

let rec loop ic oc ~lineno ~filename st =
  match input_line ic with
  | line -> (
      let next = loop ic oc ~lineno:(succ lineno) ~filename in
      match String.trim line with
      | "#else" -> (
          match State.triggered_before st with
          | true -> next (State.don't_print st)
          | false -> next (State.conditional_triggered st))
      | "#endif" -> next (State.end_conditional st)
      | trimmed_line when is_define_statement trimmed_line ->
          let var = variable_of_define trimmed_line in
          let st = State.define var st in
          next st
      | trimmed_line when is_undef_statement trimmed_line ->
          let var = variable_of_undef trimmed_line in
          let st = State.undefine var st in
          next st
      | trimmed_line when is_include_statement trimmed_line ->
          let filename = filename_of_include trimmed_line in
          let included_ic = open_in filename in
          loop included_ic oc ~lineno:1 ~filename st;
          next st
      | trimmed_line when is_ifdef trimmed_line ->
          let var = variable_of_ifdef trimmed_line in
          let is_defined = State.is_defined var st in
          let st = State.start_conditional is_defined st in
          next st
      | trimmed_line when is_if_statement trimmed_line -> (
          match is_ocaml_version line with
          | None ->
              failwith
                (Printf.sprintf "Parsing #if in file %s line %d failed, exiting"
                   filename lineno)
          | Some (major, minor, patch) ->
              next
                (State.start_conditional
                   (greater_or_equal (major, minor, patch))
                   st))
      | trimmed_line when is_elif_defined_statement trimmed_line -> (
          match State.triggered_before st with
          | true -> next (State.don't_print st)
          | false -> (
              let var = variable_of_elif_defined trimmed_line in
              match State.is_defined var st with
              | true -> next (State.conditional_triggered st)
              | false -> next st))
      | _trimmed_line ->
          if State.should_output st then output_endline oc line;
          next st)
  | exception End_of_file ->
      if not (State.finished st) then
        failwith "Output stack messed up, missing #endif?"

let () =
  let output_file = ref None in
  let input_file = ref None in
  let speclist =
    [
      ( "-o",
        Arg.String (fun filename -> output_file := Some filename),
        "Set output file name" );
    ]
  in
  let anonymous filename = input_file := Some filename in
  let usage = "mucppo -o <output> <file>" in
  Arg.parse speclist anonymous usage;
  let ic, filename =
    match !input_file with
    | Some filename -> (open_in filename, filename)
    | None -> (stdin, "<stdin>")
  in
  let oc =
    match !output_file with
    | Some filename -> open_out filename
    | None -> stdout
  in
  loop ic oc ~lineno:1 ~filename State.empty;
  close_in ic;
  close_out oc
