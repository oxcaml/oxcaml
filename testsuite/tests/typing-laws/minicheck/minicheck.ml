(* A tiny backend: a choice is a list of cases, and a law is checked on
   every combination of the cases of its choices. *)

module Choice = struct
  type 'a t = { cases : 'a list; show : 'a -> string }
end

module Law = CamlinternalLaw.Make (Choice)

let choice ?(show = fun _ -> "_") cases = { Choice.cases; show }

exception Failed of string

(* The range of the interface at [location] (which is in the directory the
   test runs in), as written, indented under the message. *)
let excerpt { CamlinternalLaw.file; start_line; start_col; end_line; end_col } =
  let lines = In_channel.with_open_text file In_channel.input_lines in
  let line n = List.nth lines (n - 1) in
  if start_line = end_line then
    String.sub (line start_line) start_col (end_col - start_col)
  else
    let first = line start_line in
    let first = String.sub first start_col (String.length first - start_col) in
    let last = String.sub (line end_line) 0 end_col in
    let middle =
      List.init (end_line - start_line - 1) (fun i -> line (start_line + 1 + i))
    in
    String.concat "\n  " ((first :: middle) @ [last])

let check ~name ~location law =
  let passed = ref 0 and discarded = ref 0 in
  let rec run : type a. a Law.t -> (a -> unit) -> unit =
   fun law k ->
    match law with
    | Return x -> k x
    | Bind (l, f) -> run l (fun x -> run (f x) k)
    | Choose { cases; _ } -> List.iter k cases
    | Assumption { value; predicate; _ } ->
        if predicate value then k () else incr discarded
    | Assertion { value; source; predicate; location } ->
        let fail what =
          raise
            (Failed
               (Printf.sprintf "%s\n  %s\n  counterexample: %s" what
                  (excerpt location) (source.show value)))
        in
        (match predicate value with
         | true -> incr passed; k ()
         | false -> fail "FAILED"
         | exception e -> fail ("FAILED, raised " ^ Printexc.to_string e))
  in
  Printf.printf "%s\n  %s\n" name (excerpt location);
  match run law ignore with
  | () -> Printf.printf "  passed %d, discarded %d\n" !passed !discarded
  | exception Failed msg -> Printf.printf "  %s\n" msg
