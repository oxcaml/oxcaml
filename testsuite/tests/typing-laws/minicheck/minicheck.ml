(* A tiny backend: a choice is a list of cases, and a law is checked on
   every combination of the cases of its choices. *)

module Choice = struct
  type 'a t = { cases : 'a list; show : 'a -> string }
end

module Law = CamlinternalLaw.Make (Choice)

let choice ?(show = fun _ -> "_") cases = { Choice.cases; show }

exception Failed of string

let check ~name law =
  let passed = ref 0 and discarded = ref 0 in
  let rec run : type a. a Law.t -> (a -> unit) -> unit =
   fun law k ->
    match law with
    | Return x -> k x
    | Bind (l, f) -> run l (fun x -> run (f x) k)
    | Choose { cases; _ } -> List.iter k cases
    | Assumption { value; predicate; _ } ->
        if predicate value then k () else incr discarded
    | Assertion { value; source; predicate; text } ->
        let fail what =
          raise
            (Failed
               (Printf.sprintf "%s%s\n  counterexample: %s" text what
                  (source.show value)))
        in
        (match predicate value with
         | true -> incr passed; k ()
         | false -> fail ""
         | exception e -> fail (" raised " ^ Printexc.to_string e))
  in
  match run law ignore with
  | () ->
      Printf.printf "%-24s passed %d, discarded %d\n" name !passed !discarded
  | exception Failed msg -> Printf.printf "%-24s FAILED: %s\n" name msg
