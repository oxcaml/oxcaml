module D = Asm_targets.Asm_directives
module S = Asm_targets.Asm_symbol

type callee =
  | Symbol of string
  | Entry of Fdo_counter.hashed

let alias_symbol hashes =
  S.create_global
    (String.concat "_"
       ("caml_fdo"
       :: List.map
            (fun (h : Fdo_counter.Hash.t) ->
              Printf.sprintf "%08lx" (h :> int32))
            hashes))

let compare_callee a b =
  match a, b with
  | Symbol a, Symbol b -> String.compare a b
  | Symbol _, Entry _ -> -1
  | Entry _, Symbol _ -> 1
  | Entry a, Entry b -> List.compare Fdo_counter.Hash.compare a b

let callee_symbol = function
  | Symbol name -> S.create_global name
  | Entry hashes -> alias_symbol hashes

(* Edges of the current compilation unit, keyed by their endpoints. *)
module Edge_tbl = Hashtbl.Make (struct
  type t = string * callee

  let equal (from_a, a) (from_b, b) =
    String.equal from_a from_b && compare_callee a b = 0

  let hash (from, callee) =
    let callee =
      match callee with
      | Symbol name -> String.hash name
      | Entry hashes ->
        List.fold_left
          (fun acc (h : Fdo_counter.Hash.t) ->
            (acc * 31) + Int32.to_int (h :> int32))
          1 hashes
    in
    (String.hash from * 31) + callee
end)

let edges : int64 Edge_tbl.t = Edge_tbl.create 64

let add_edge ~from ~callee ~weight =
  let key = from, callee in
  let existing = Option.value (Edge_tbl.find_opt edges key) ~default:0L in
  Edge_tbl.replace edges key (Int64.add existing weight)

let reset () = Edge_tbl.reset edges

(* The section is SHT_LLVM_CALL_GRAPH_PROFILE (0x6fff4c09) with SHF_EXCLUDE (not
   linked into the output) and SHF_MERGE with an entry size of 8, as lld
   requires. Each entry is the 8-byte weight, preceded at the same offset by two
   R_X86_64_NONE relocations: the caller, then the callee. *)
let section =
  Asm_targets.Asm_section.Custom
    { names = [".llvm.call_graph_profile"];
      flags = Some "eM";
      args = ["@0x6fff4c09"; "8"];
      is_delayed = false
    }

let emit_section () =
  if Edge_tbl.length edges > 0
  then (
    let sorted =
      Edge_tbl.fold (fun key weight acc -> (key, weight) :: acc) edges []
      |> List.sort (fun ((from_a, a), _) ((from_b, b), _) ->
          let c = String.compare from_a from_b in
          if c <> 0 then c else compare_callee a b)
    in
    (* Aliases are defined by the callee's compilation unit only when the
       profile knows the function: reference them weakly, so that an edge to a
       function the linker never sees is dropped rather than an error. *)
    List.filter_map
      (fun ((_, callee), _) ->
        match callee with Entry hashes -> Some hashes | Symbol _ -> None)
      sorted
    |> List.sort_uniq (List.compare Fdo_counter.Hash.compare)
    |> List.iter (fun hashes -> D.weak (alias_symbol hashes));
    D.switch_to_section ~emit_label_on_first_occurrence:false section;
    List.iter
      (fun ((from, callee), weight) ->
        D.reloc_x86_64_none ~target_symbol:(S.create_global from);
        D.reloc_x86_64_none ~target_symbol:(callee_symbol callee);
        D.int64 weight)
      sorted;
    reset ())
