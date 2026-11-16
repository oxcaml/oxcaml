(* A backend that runs a law on the inputs that a test gives, and prints the
   law, as written in its source file, followed by a trace of its clauses. A
   choice is a list of inputs, each with a label to print. It prints with
   [Format], so that expect tests capture the output. *)

module Choice = struct
  type 'a t = (string * 'a) list
end

module Law = CamlinternalLaw.Make (Choice)

(* Traces *)

type kind =
  | Assume
  | Assert

type outcome =
  | Holds
  | Fails
  | Raises of exn

type step =
  | Input of string * step list  (** An input and the clauses checked on it *)
  | Clause of
      { kind : kind;
        location : CamlinternalLaw.location;
        outcome : outcome;
        from_choice : bool  (** The value is an input of its choice. *)
      }

let clause kind ~value ~source ~predicate ~location =
  let outcome =
    match predicate value with
    | true -> Holds
    | false -> Fails
    | exception e -> Raises e
  in
  let from_choice = List.exists (fun (_, input) -> input == value) source in
  Clause { kind; location; outcome; from_choice }

(* The steps of [law] on every input of its choices, followed by the steps of
   [k] on its result. An assumption that does not hold discards the input:
   the clauses after it are not checked. *)
let rec trace : type a. a Law.t -> (a -> step list) -> step list =
 fun law k ->
  match law with
  | Return x -> k x
  | Bind (law, f) -> trace law (fun x -> trace (f x) k)
  | Choose inputs ->
      List.map (fun (label, input) -> Input (label, k input)) inputs
  | Assumption { value; source; predicate; location } -> (
      match clause Assume ~value ~source ~predicate ~location with
      | Clause { outcome = Holds; _ } as step -> step :: k ()
      | step -> [ step ])
  | Assertion { value; source; predicate; location } ->
      clause Assert ~value ~source ~predicate ~location :: k ()

(* Printing *)

let leading_spaces line =
  let rec go i =
    if i < String.length line && line.[i] = ' ' then go (i + 1) else i
  in
  go 0

(* The lines of the text at [location], read back from its file, which is
   in the current directory. The lines after the first lose up to
   [start_col] leading spaces, so that they keep their indentation relative
   to the first. *)
let excerpt { CamlinternalLaw.file; start_line; start_col; end_line; end_col }
    =
  let lines = In_channel.with_open_text file In_channel.input_lines in
  let lines =
    List.filteri (fun i _ -> start_line <= i + 1 && i < end_line) lines
  in
  let last = List.length lines - 1 in
  List.mapi
    (fun i line ->
      let start =
        if i = 0 then start_col else min start_col (leading_spaces line)
      in
      let stop = if i = last then end_col else String.length line in
      String.sub line start (stop - start))
    lines

(* The lines are separated by cuts of the enclosing vertical box. *)
let pp_lines ppf lines =
  Format.pp_print_list ~pp_sep:Format.pp_print_cut Format.pp_print_string ppf
    lines

let pp_kind ppf = function
  | Assume -> Format.pp_print_string ppf "assume"
  | Assert -> Format.pp_print_string ppf "assert"

let pp_outcome ppf = function
  | Holds -> Format.pp_print_string ppf "holds"
  | Fails -> Format.pp_print_string ppf "fails"
  | Raises e -> Format.fprintf ppf "raises %s" (Printexc.to_string e)

let rec pp_step ppf = function
  | Input (label, steps) ->
      Format.fprintf ppf "@[<v 2>input %s%a@]" label pp_steps steps
  | Clause { kind; location; outcome; from_choice } ->
      Format.fprintf ppf "@[<v 2>%a %a: %a%s@]" pp_kind kind pp_lines
        (excerpt location) pp_outcome outcome
        (if from_choice then "" else " (not on an input of its choice)")

and pp_steps ppf steps =
  List.iter (fun step -> Format.fprintf ppf "@,%a" pp_step step) steps

(* [law ~names ~locations laws], given [to_list] of the [names], [locations]
   and [laws] of a laws file, traces the law with a given name. *)
let law ~names ~locations laws =
  let laws = List.combine names (List.combine locations laws) in
  fun name ->
    let location, law = List.assoc name laws in
    Format.printf "@[<v 2>%a%a@]@." pp_lines (excerpt location) pp_steps
      (trace law (fun () -> []))
