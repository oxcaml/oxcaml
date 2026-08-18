type problem =
  | Unreadable of { path : string; message : string }
  | Malformed of { path : string; message : string }

type status =
  { facts_present : bool;
    channels_loaded : int;
    sources_folded : int;
    problems : problem list
  }

let pp_problem fmt = function
  | Unreadable { path; message } ->
    Format.fprintf fmt "unreadable %s: %s" path message
  | Malformed { path; message } ->
    Format.fprintf fmt "malformed facts in %s: %s" path message

let facts_is_empty (facts : Module_implementation_facts.t) =
  match facts with
  | { checks = []; dependencies = []; equalities = []; omissions = [] } -> true
  | _ -> false

exception Not_an_index_file

type decoded =
  { facts : Module_implementation_facts.t;
    facts_present : bool;
    malformed : string option
  }

module Decoded_cache = File_cache.Make (struct
  type t = decoded

  let read file =
    match Index_format.read ~file with
    | Cmt _ | Cms _ | Unknown -> raise Not_an_index_file
    | Index index -> (
      let facts_present = index.module_facts_present in
      let block = Index_format.module_facts_block index.module_facts in
      match Module_facts_compact.to_facts block with
      | Ok facts -> { facts; facts_present; malformed = None }
      | Error message ->
        { facts = Module_implementation_facts.empty;
          facts_present;
          malformed = Some message
        })

  let cache_name = "Module_facts_reader"
end)

type 'acc state =
  { mutable accumulator : 'acc;
    mutable present : bool;
    mutable channels_loaded : int;
    mutable sources_folded : int;
    mutable problems_rev : problem list
  }

let fold ~index_files ~init ~f =
  let state =
    { accumulator = init;
      present = true;
      channels_loaded = 0;
      sources_folded = 0;
      problems_rev = []
    }
  in
  let report problem =
    state.problems_rev <- problem :: state.problems_rev;
    state.present <- false
  in
  let consume ~path decoded =
    state.present <- state.present && decoded.facts_present;
    match decoded.malformed with
    | Some message -> report (Malformed { path; message })
    | None ->
      if decoded.facts_present then
        state.channels_loaded <- state.channels_loaded + 1;
      if not (facts_is_empty decoded.facts) then begin
        state.sources_folded <- state.sources_folded + 1;
        state.accumulator <- f state.accumulator ~path decoded.facts
      end
  in
  List.iter
    (fun path ->
      match Decoded_cache.read path with
      | exception Not_an_index_file ->
        report (Unreadable { path; message = "not an index file" })
      | exception exn ->
        report (Unreadable { path; message = Printexc.to_string exn })
      | decoded -> consume ~path decoded)
    index_files;
  ( state.accumulator,
    { facts_present = state.present;
      channels_loaded = state.channels_loaded;
      sources_folded = state.sources_folded;
      problems = List.rev state.problems_rev
    } )

let flush ?older_than () = Decoded_cache.flush ?older_than ()

let load ~index_files =
  let runs, status =
    fold ~index_files ~init:[] ~f:(fun runs ~path:_ facts -> facts :: runs)
  in
  (Module_implementation_facts.merge_many (List.rev runs), status)
