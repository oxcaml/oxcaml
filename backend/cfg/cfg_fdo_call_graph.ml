[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare

(* The call graph edges of the function for the linker (see [Fdo_call_graph]):
   every real call instruction in a block with a positive count, weighted by
   that count. The callees are the functions the profile saw the call site reach
   (its call-target index, each weighted by the count the trie has for the call
   site in as much of its inlining context as recorded, the block count split in
   proportion), so that calls through the runtime's stubs (caml_applyN and
   friends) and indirect calls resolve to the actual functions; a call the
   profile knows nothing about keeps its static callee if it has one. Self tail
   calls and external calls are not calls of the call graph.

   CR-someday ttebbi: the block counts of a function aggregate over its inlined
   copies (a counter's root sums all inlining contexts), so a function that is
   only ever inlined, or mostly so, still has hot blocks here and produces hot
   edges, e.g. from a functor's [@@inline always] function to the stubs of its
   calls through functor arguments. Whether the standalone copy is really hot
   depends on the inlining decisions of other compilation units, which are not
   known here; only a global computation of the weights could tell. *)
let record ~dump profile counts (cfg : Cfg.t) =
  Cfg.iter_blocks cfg ~f:(fun label block ->
      let weight = Cfg_fdo_counts.block_count counts label in
      let callee : Cfg.func_call_operation option =
        match block.terminator.desc with
        | Call { op; label_after = _ } | Tailcall_func op -> Some op
        | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _
        | Int_test _ | Switch _ | Return | Raise _ | Tailcall_self _
        | Call_no_return _ | Prim _ | Invalid _ ->
          None
      in
      match callee with
      | None -> ()
      | Some _ when Int64.compare weight 0L <= 0 -> ()
      | Some op -> (
        let callsite_counter =
          match op with
          | Direct { sym = _; callsite_counter }
          | Indirect { callees = _; callsite_counter } ->
            callsite_counter
        in
        let from_profile =
          match callsite_counter with
          | None -> []
          | Some callsite ->
            let context = Fdo_counter.hash callsite in
            List.filter_map
              (fun root ->
                let count =
                  Source_position_profile.count_for_deepest_context profile
                    ~root ~context
                in
                if Int64.compare count 0L > 0 then Some (root, count) else None)
              (Source_position_profile.call_targets profile callsite.position)
        in
        let add (callee : Fdo_call_graph.callee) weight =
          Option.iter
            (fun ppf ->
              Format.fprintf ppf "  call from block %a%a to %s: %Ld@."
                Label.format label
                (fun ppf callsite ->
                  Option.iter
                    (fun callsite ->
                      Format.fprintf ppf " [%a]" Cfg_fdo_counts.print_counter
                        callsite)
                    callsite)
                callsite_counter
                (Asm_targets.Asm_symbol.encode
                   (match callee with
                   | Symbol name -> Asm_targets.Asm_symbol.create_global name
                   | Entry hashes -> Fdo_call_graph.alias_symbol hashes))
                weight)
            dump;
          Fdo_call_graph.add_edge ~from:cfg.fun_name ~callee ~weight
        in
        match from_profile, op with
        | [], Direct { sym = func; callsite_counter = _ } ->
          add (Symbol func.sym_name) weight
        | [], Indirect _ -> ()
        | targets, (Direct _ | Indirect _) ->
          let total =
            List.fold_left (fun acc (_, n) -> Int64.add acc n) 0L targets
          in
          List.iter
            (fun (root, n) ->
              let share = Int64.div (Int64.mul weight n) total in
              if Int64.compare share 0L > 0 then add (Entry [root]) share)
            targets))
