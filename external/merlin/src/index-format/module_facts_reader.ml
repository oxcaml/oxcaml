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

exception Not_an_index_file

type decoded =
  { facts : Module_implementation_facts.t list;
    facts_present : bool;
    malformed : string option
  }

module Decoded_cache = File_cache.Make (struct
  type t = decoded

  let read file =
    match Index_format.read ~file with
    | Cmt _ | Cms _ | Unknown -> raise Not_an_index_file
    | Index index -> (
      match index.module_facts with
      | None -> { facts = []; facts_present = false; malformed = None }
      | Some module_facts -> (
        match Index_format.module_facts_list module_facts with
        | facts -> { facts; facts_present = true; malformed = None }
        | exception exn ->
          { facts = [];
            facts_present = true;
            malformed = Some (Printexc.to_string exn)
          }))

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
      List.iter
        (fun facts ->
          state.sources_folded <- state.sources_folded + 1;
          state.accumulator <- f state.accumulator ~path facts)
        decoded.facts
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
