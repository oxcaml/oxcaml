open Browse_raw
open Std

type syntax_info = Query_protocol.Syntax_doc_result.t option

module Doc_website_base = struct
  type t = Ocaml | Oxcaml
end

let syntax_doc_url (doc_website_base : Doc_website_base.t) endpoint =
  let base_url =
    match doc_website_base with
    | Ocaml -> "https://ocaml.org/manual/5.2/"
    | Oxcaml -> "https://oxcaml.org/documentation/"
  in
  Some (base_url ^ endpoint)

(** Drop elements from the head of [list] until [f] returns [true]. *)
let rec drop_until list ~f =
  match list with
  | [] -> []
  | hd :: rest -> (
    match f hd with
    | true -> list
    | false -> drop_until rest ~f)

module Loc_comparison_result = struct
  type t = Before | Inside | After | Ghost

  let is_inside = function
    | Before | After | Ghost -> false
    | Inside -> true
end

let get_jkind_abbrev_doc abbrev =
  let open Option.Infix in
  let open struct
    type docpage = Kind_syntax | Unboxed_types
  end in
  let* description, docpage =
    match abbrev with
    | "any" ->
      Some
        ("The top of the kind lattice; all types have this kind.", Kind_syntax)
    | "any_non_null" -> Some ("A synonym for `any mod non_null`.", Kind_syntax)
    | "value_or_null" ->
      Some
        ( "The kind of ordinary OCaml types, but with the possibility that the \
           type contains `null`.",
          Kind_syntax )
    | "value" -> Some ("The kind of ordinary OCaml types", Kind_syntax)
    | "void" ->
      Some
        ( "The layout of types that are represented by 0 bits at runtime; \
           these types can contain only 1 value.",
          Kind_syntax )
    | "immediate64" ->
      Some
        ( "On 64-bit platforms, the kind of types inhabited only by tagged \
           integers.",
          Kind_syntax )
    | "immediate" ->
      Some ("The kind of types inhabited only by tagged integers.", Kind_syntax)
    | "immediate_or_null" ->
      Some
        ( "The kind of types inhabited by tagged integers and the bit pattern \
           containing all 0s.",
          Kind_syntax )
    | "float64" ->
      Some
        ( "The layout of types represented by a 64-bit machine float.",
          Unboxed_types )
    | "float32" ->
      Some
        ( "The layout of types represented by a 32-bit machine float.",
          Unboxed_types )
    | "word" ->
      Some
        ( "The layout of types represented by a native-width machine word.",
          Unboxed_types )
    | "bits8" ->
      Some
        ( "The layout of types represented by an 8-bit machine word.",
          Unboxed_types )
    | "bits16" ->
      Some
        ( "The layout of types represented by a 16-bit machine word.",
          Unboxed_types )
    | "bits32" ->
      Some
        ( "The layout of types represented by a 32-bit machine word.",
          Unboxed_types )
    | "bits64" ->
      Some
        ( "The layout of types represented by a 64-bit machine word.",
          Unboxed_types )
    | "vec128" ->
      Some
        ( "The layout of types represented by a 128-bit machine vector.",
          Unboxed_types )
    | "vec256" ->
      Some
        ( "The layout of types represented by a 256-bit machine vector.",
          Unboxed_types )
    | "vec512" ->
      Some
        ( "The layout of types represented by a 512-bit machine vector.",
          Unboxed_types )
    | "immutable_data" ->
      Some
        ( "The kind of types that contain no mutable parts and no functions.",
          Kind_syntax )
    | "sync_data" ->
      Some
        ( "The kind of types that contain no mutable parts (except possibly \
           for atomic fields) and no functions.",
          Kind_syntax )
    | "mutable_data" ->
      Some
        ( "The kind of types that may have mutable parts but contain no \
           functions.",
          Kind_syntax )
    | _ -> None
  in
  let docpage_str =
    match docpage with
    | Kind_syntax -> "kinds/syntax/"
    | Unboxed_types -> "unboxed-types/intro/"
  in
  (Some
     { name = "Kind abbreviation";
       description;
       documentation = syntax_doc_url Oxcaml docpage_str;
       level = Advanced
     }
    : syntax_info)

let get_mod_bound_doc mod_bound =
  let open Option.Infix in
  let open struct
    type parse_result =
      | Axis_pair : 'a Jkind_axis.Axis.t * 'a -> parse_result
      | Everything
  end in
  let* parsed =
    match Typemode.Modifier_axis_pair.of_string mod_bound with
    | exception Not_found -> (
      match mod_bound with
      | "everything" -> Some Everything
      | __ -> None)
    | P (axis, bound) -> Some (Axis_pair (axis, bound))
  in
  let* description =
    match parsed with
    | Axis_pair (Modal (Comonadic _), _) ->
      Some
        (Format.asprintf
           "Values of types of this kind can cross to `%s` from weaker modes."
           mod_bound)
    | Axis_pair (Modal (Monadic _), _) ->
      Some
        (Format.asprintf
           "Values of types of this kind can cross from `%s` to stronger modes"
           mod_bound)
    | Axis_pair (Nonmodal Externality, Internal) ->
      Some "Values of types of this kind might be pointers to the OCaml heap"
    | Axis_pair (Nonmodal Externality, External64) ->
      Some
        "On 64-bit systems, values of types of this kind are never pointers to \
         the OCaml heap"
    | Axis_pair (Nonmodal Externality, External) ->
      Some "Values of types of this kind are never pointers to the OCaml heap"
    | Axis_pair (Nonmodal Nullability, Maybe_null) ->
      Some
        "Values of types of this kind might be the bit pattern containing all \
         0s"
    | Axis_pair (Nonmodal Nullability, Non_null) ->
      Some
        "Values of types of this kind that are also a subkind of `value` are \
         never the bit pattern containing all 0s"
    | Axis_pair (Nonmodal Separability, Non_float) ->
      Some "Values of types of this kind are never pointers to floats."
    | Axis_pair (Nonmodal Separability, Separable) ->
      Some
        "No type of this kind includes both pointers to a float and other \
         values."
    | Axis_pair (Nonmodal Separability, Maybe_separable) ->
      Some "Types of this kind may mix pointers to floats with other values."
    | Everything ->
      Some
        "Synonym for \"global aliased many contended portable unyielding \
         immutable stateless external_\", convenient for describing \
         immediates."
  in
  (Some
     { name = "Mod-bound";
       description;
       documentation = syntax_doc_url Oxcaml "kinds/intro/";
       level = Advanced
     }
    : syntax_info)

let get_mode_doc mode =
  let open Option.Infix in
  let* (P (axis, mode)) =
    match Typemode.Mode_axis_pair.of_string mode with
    | exception Not_found -> None
    | res -> Some res
  in
  let* description =
    match (axis, mode) with
    | Comonadic Areality, Local ->
      Some "Values with this mode cannot escape the current region"
    | Comonadic Areality, Global ->
      Some "Values with this mode can escape any region"
    | Monadic Contention, Contended ->
      Some
        "The mutable parts of values with this mode cannot be accessed (unless \
         they are atomic)"
    | Monadic Contention, Shared ->
      Some
        "The mutable parts of values with this mode can be read, but not \
         written (unless they are atomic)"
    | Monadic Contention, Uncontended ->
      Some "The mutable parts of values with this mode can be fully accessed"
    | Comonadic Portability, Nonportable ->
      Some
        "Values with this mode cannot be sent to or shared with other threads, \
         in order to avoid data races."
    | Comonadic Portability, Shareable ->
      Some
        "Values with this mode can be shared with (but not sent to) other \
         threads without causing data races"
    | Comonadic Portability, Portable ->
      Some
        "Values with this mode can be sent to other threads without causing \
         data races"
    | Monadic Uniqueness, Aliased ->
      Some "There may be multiple pointers to values with this mode"
    | Monadic Uniqueness, Unique ->
      Some
        "It is guaranteed that there is only one pointer to values with this \
         mode"
    | Comonadic Linearity, Once ->
      Some "Functions with this mode can only be called once"
    | Comonadic Linearity, Many ->
      Some "Functions with this mode can be called any number of times"
    | Comonadic Yielding, Yielding ->
      Some "Functions with this mode can jump to effect handlers"
    | Comonadic Yielding, Unyielding ->
      Some "Functions within this value will never jump to an effect handler"
    | Monadic Visibility, Immutable ->
      Some "The mutable parts of values with this mode cannot be accessed"
    | Monadic Visibility, Read ->
      Some
        "The mutable parts of values with this mode can be read, but not \
         written"
    | Monadic Visibility, Read_write ->
      Some "The mutable parts of values with this mode can be fully accessed"
    | Comonadic Statefulness, Stateful ->
      Some "Functions with this mode can read and write mutable data"
    | Comonadic Statefulness, Observing ->
      Some "Functions with this mode can read but not write mutable data"
    | Comonadic Statefulness, Stateless ->
      Some "Functions with this mode cannot access mutable data"
    | Comonadic Forkable, Forkable ->
      Some "Functions with this mode may be executed concurrently."
    | Comonadic Forkable, Unforkable ->
      Some "Functions with this mode cannot be executed concurrently."
    | Monadic Staticity, Static -> Some "The value is known at compile-time."
    | Monadic Staticity, Dynamic ->
      Some "The value is not known at compile-time."
  in
  let doc_url =
    let subpage =
      match axis with
      | Comonadic Areality -> "stack-allocation/intro/"
      | Monadic Contention -> "parallelism/01-intro/"
      | Comonadic Portability -> "parallelism/01-intro/"
      | Monadic Uniqueness -> "uniqueness/intro/"
      | Comonadic Linearity -> "uniqueness/intro/"
      | Comonadic Yielding -> "modes/intro/"
      | Monadic Visibility -> "modes/intro/"
      | Comonadic Statefulness -> "modes/intro/"
      | Comonadic Forkable -> "modes/intro/"
      | Monadic Staticity -> "modes/intro/"
    in
    syntax_doc_url Oxcaml subpage
  in
  (Some
     { name = "Mode"; description; documentation = doc_url; level = Advanced }
    : syntax_info)

let get_modality_doc modality =
  let open Option.Infix in
  let* (P (axis, _)) =
    match Typemode.Modality_axis_pair.of_string modality with
    | exception Not_found -> None
    | res -> Some res
  in
  let description =
    (* CR-someday: Detect the context that the modality is within to make this message
       more detailed. Ex: "This field is always stronger than _, even if the record has a
       weaker mode." *)
    match axis with
    | Comonadic _ ->
      Format.asprintf
        "The annotated value's mode is always at least as strong as `%s`, even \
         if its container's mode is weaker."
        modality
    | Monadic _ ->
      Format.asprintf
        "The annotated value's mode is always at least as weak as `%s`, even \
         if its container's mode is a stronger."
        modality
  in
  (Some
     { name = "Modality";
       description;
       documentation = syntax_doc_url Oxcaml "modes/syntax/";
       level = Advanced
     }
    : syntax_info)

let get_oxcaml_syntax_doc cursor_loc nodes : syntax_info =
  (* Merlin-jst specific: This function gets documentation for oxcaml language
     extensions. *)
  let compare_cursor_to_loc (loc : Location.t) : Loc_comparison_result.t =
    match loc.loc_ghost with
    | true -> Ghost
    | false -> (
      match Location_aux.compare_pos cursor_loc loc with
      | n when n < 0 -> Before
      | n when n > 0 -> After
      | _ -> Inside)
  in
  let nodes = List.map nodes ~f:snd in
  let nodes =
    (* Sometimes the bottom node of [nodes] doesn't include the location of the cursor.
       This seems to be because Merlin will find the bottom-most node that contains the
       cursor, but then select a child of that node via some heuristics. This is in order
       to try to find a node with the environment the user most likely wanted if they,
       say, have their cursor on a keyword that isn't represented by a node type in
       [Browse_raw.t] (see docstring on [Mtyper.node_at] for more info). But here we
       actually want the cursor to be included within all the nodes in [nodes] so that we
       can more easily reason about [nodes]. So we drop nodes from the head of [nodes]
       until we reach one that includes the cursor. *)
    drop_until nodes ~f:(fun node ->
        let loc = Browse_raw.node_merlin_loc Location.none node in
        Loc_comparison_result.is_inside (compare_cursor_to_loc loc))
  in
  let stack_allocation_url =
    syntax_doc_url Oxcaml "stack-allocation/reference/"
  in
  let get_doc_for_attribute (attribute : Parsetree.attribute) : syntax_info =
    let builtin_attrs_doc_url =
      syntax_doc_url Ocaml "attributes.html#ss:builtin-attributes"
    in
    (* See below usage of this function for explanation of why this isn't part of the
       other big match statement. *)
    match attribute with
    (* Zero-alloc annotations *)
    | { attr_name = { txt = "zero_alloc"; _ }; attr_payload; _ } -> (
      let doc_url =
        syntax_doc_url Oxcaml "miscellaneous-extensions/zero_alloc_check/"
      in
      match attr_payload with
      | PStr [] ->
        Some
          { name = "Zero-alloc annotation";
            description =
              "This function does not allocate on the OCaml heap on executions \
               that return normally. The function may allocate if it raises an \
               exception.";
            documentation = doc_url;
            level = Advanced
          }
      | PStr
          [ { pstr_desc =
                Pstr_eval
                  ( { pexp_desc =
                        ( Pexp_ident { txt = Lident zero_alloc_flag_name; _ }
                        | Pexp_apply
                            ( { pexp_desc =
                                  Pexp_ident
                                    { txt = Lident zero_alloc_flag_name; _ };
                                _
                              },
                              _ ) );
                      _
                    },
                    _ );
              _
            }
          ] -> (
        match zero_alloc_flag_name with
        | "opt" ->
          Some
            { name = "Zero-alloc opt annotation";
              description =
                "Same as [@zero_alloc], but checks during optimized builds \
                 only.";
              documentation = doc_url;
              level = Advanced
            }
        | "assume" ->
          Some
            { name = "Zero-alloc assume annotation";
              description =
                "This function is assumed to be zero-alloc, but the compiler \
                 does not guarantee it.";
              documentation = doc_url;
              level = Advanced
            }
        | "assume_unless_opt" ->
          Some
            { name = "Zero-alloc assume_unless_opt annotation";
              description =
                "Same as [@zero_alloc opt] in optimized builds. Same as \
                 [@zero_alloc assume] in non-optimized builds.";
              documentation = doc_url;
              level = Advanced
            }
        | "strict" ->
          Some
            { name = "Zero-alloc strict annotation";
              description =
                "This function does not allocate on the OCaml heap (both \
                 normal and exceptional returns).";
              documentation = doc_url;
              level = Advanced
            }
        | "arity" ->
          Some
            { name = "Zero-alloc arity annotation";
              description =
                "The function does not allocate when applied to [n] arguments. \
                 This can be used to override the arity inferred based on the \
                 number of arrows in the type.";
              documentation = doc_url;
              level = Advanced
            }
        | _ -> None)
      | _ ->
        Some
          { name = "Unrecognized zero-alloc annotation";
            description = "This is an unrecognized zero-alloc annotation.";
            documentation = doc_url;
            level = Advanced
          })
    | { attr_name = { txt = "noalloc"; _ }; _ } ->
      Some
        { name = "Noalloc annotation";
          description =
            "This external does not allocate, does not raise exceptions, and \
             does not release the domain lock. The compiler will optimize uses \
             to a direct C call.";
          documentation = syntax_doc_url Ocaml "intfc.html#ss:c-direct-call";
          level = Advanced
        }
    (* Inlining annotations *)
    | { attr_name = { txt = "inline"; _ }; attr_payload; _ } -> (
      let inline_always_annot : syntax_info =
        Some
          { name = "Inline always annotation";
            description =
              "On a function declaration, causes the function to be inlined at \
               all known call sites (can be overridden by [@inlined]).  In \
               addition it will be made available for inlining in other source \
               files (with appropriate build settings permitting .cmx file \
               visibility)";
            documentation = builtin_attrs_doc_url;
            level = Advanced
          }
      in
      match attr_payload with
      | PStr [] -> inline_always_annot
      | PStr
          [ { pstr_desc =
                Pstr_eval
                  ( { pexp_desc = Pexp_ident { txt = Lident inline_flag_name; _ };
                      _
                    },
                    _ );
              _
            }
          ] -> (
        match inline_flag_name with
        | "always" -> inline_always_annot
        | "never" ->
          Some
            { name = "Inline never annotation";
              description =
                "This function will not be inlined. In this file (only), this \
                 can be overridden at call sites with [@inlined].";
              documentation = builtin_attrs_doc_url;
              level = Advanced
            }
        | "available" ->
          Some
            { name = "Inline available annotation";
              description =
                "Causes the function to be available for inlining in other \
                 source files, but does not affect actual inlining decisions.  \
                 Can be used to ensure cross-source-file inlining even in \
                 cases where it would normally be unavailable e.g. a very \
                 large function";
              documentation = None;
              level = Advanced
            }
        | _ -> None)
      | _ ->
        Some
          { name = "Unrecognized inline annotation";
            description = "Unrecognized inline annoation";
            documentation = builtin_attrs_doc_url;
            level = Advanced
          })
    | { attr_name = { txt = "inlined"; _ }; attr_payload; _ } -> (
      let inlined_always_annot : syntax_info =
        Some
          { name = "Inlined always annotation";
            description =
              "If possible, this function call will be inlined.  The function \
               must be known to the optimizer (i.e. not an indirect call; and \
               if in another source file, the .cmx for that file must be \
               available and the function available for inlining e.g. by \
               [@inline always] or [@inline available] or the decision of the \
               optimizer).  This attribute can override [@inline never] but \
               only within the same source file.";
            documentation = builtin_attrs_doc_url;
            level = Advanced
          }
      in
      match attr_payload with
      | PStr [] -> inlined_always_annot
      | PStr
          [ { pstr_desc =
                Pstr_eval
                  ( { pexp_desc = Pexp_ident { txt = Lident inline_flag_name; _ };
                      _
                    },
                    _ );
              _
            }
          ] -> (
        match inline_flag_name with
        | "always" -> inlined_always_annot
        | "never" ->
          Some
            { name = "Inlined never annotation";
              description =
                "This function call will not be inlined, overriding any \
                 attribute on the function's declaration.";
              documentation = builtin_attrs_doc_url;
              level = Advanced
            }
        | "hint" ->
          Some
            { name = "Inlined hint annotation";
              description =
                "If possible, this function call will be inlined, like \
                 [@inlined always]. However, no warning is emitted when \
                 inlining is not possible.";
              documentation = None;
              level = Advanced
            }
        | _ -> None)
      | _ ->
        Some
          { name = "Unrecognized inlined annotation";
            description = "Unrecognized inlined annotation";
            documentation = builtin_attrs_doc_url;
            level = Advanced
          })
    | { attr_name = { txt = "loop"; _ }; attr_payload; _ } -> (
      let loop_always_desc : syntax_info =
        Some
          { name = "Loop always annotation";
            description =
              "Forces the self-tail-recursive call sites, if any, in the given \
               function to be converted into a loop.  If those are the only \
               uses of the recursively-defined function variable, no closure \
               will be generated, and the function can then be inlined as a \
               loop.  This transformation is not yet supported for \
               mutually-recursive functions.";
            documentation = None;
            level = Advanced
          }
      in
      match attr_payload with
      | PStr [] -> loop_always_desc
      | PStr
          [ { pstr_desc =
                Pstr_eval
                  ( { pexp_desc = Pexp_ident { txt = Lident loop_flag_name; _ };
                      _
                    },
                    _ );
              _
            }
          ] -> (
        match loop_flag_name with
        | "always" -> loop_always_desc
        | "never" ->
          Some
            { name = "Loop never annotation";
              description =
                "Prevents the given function from being turned into a loop.";
              documentation = None;
              level = Advanced
            }
        | _ -> None)
      | _ ->
        Some
          { name = "Unrecognized loop annotation";
            description = "Unrecognized loop annotation";
            documentation = None;
            level = Advanced
          })
    | { attr_name = { txt = "unrolled"; _ }; _ } ->
      Some
        { name = "unrolled annotation";
          description =
            "On a recursive function's call site, causes the function body to \
             be unrolled this many times.  At present this is not supported if \
             the function was loopified (use [@loop never] to disable).  If in \
             another source file, the function must be available for inlining \
             e.g. by [@inline available] with the .cmx file available.";
          documentation = builtin_attrs_doc_url;
          level = Advanced
        }
    (* Misc *)
    | { attr_name = { txt = "nontail"; _ }; _ } ->
      Some
        { name = "nontail annotation";
          description =
            "This function call will be called normally (with a fresh stack \
             frame), despite appearing in tail position";
          documentation = stack_allocation_url;
          level = Advanced
        }
    | _ -> None
  in
  match nodes with
  (* Modes and modalities *)
  | Mode { txt = Mode mode; _ } :: ancestors -> (
    match ancestors with
    | Jkind_annotation _ :: _ -> get_mod_bound_doc mode
    | _ -> get_mode_doc mode)
  | Modality { txt = Modality modality; _ } :: ancestors -> (
    match ancestors with
    | Jkind_annotation _ :: _ ->
      (* CR-someday: Provide separate documatation for modalities within a jkind *)
      get_modality_doc modality
    | _ -> get_modality_doc modality)
  (* Jkinds *)
  | Jkind_annotation { pjkind_desc = Pjk_abbreviation abbrev; _ } :: _ ->
    get_jkind_abbrev_doc abbrev
  | Jkind_annotation { pjkind_desc = Pjk_mod _; _ } :: _ ->
    Some
      { name = "`mod` keyword (in a kind)";
        description = "Types of this kind will cross the following modes";
        documentation = syntax_doc_url Oxcaml "kinds/intro/";
        level = Advanced
      }
  | Jkind_annotation { pjkind_desc = Pjk_with (_, with_type, _); _ } :: _ -> (
    match compare_cursor_to_loc with_type.ptyp_loc with
    | Before ->
      Some
        { name = "`with` keyword (in a kind)";
          description =
            "Mark a type as structurally included within another; if the \
             with-type does not cross a certain mode, neither does its \
             containing type";
          documentation = syntax_doc_url Oxcaml "kinds/intro/";
          level = Advanced
        }
    | Inside ->
      Some
        { name = "with-type";
          description =
            "Mark a type as structurally included within another; if the \
             with-type does not cross a certain mode, neither does its \
             containing type";
          documentation = syntax_doc_url Oxcaml "kinds/intro/";
          level = Advanced
        }
    | After ->
      Some
        { name = "`@@` keyword (in a kind)";
          description = "Mark a type as included under a modality";
          documentation = syntax_doc_url Oxcaml "kinds/intro/";
          level = Advanced
        }
    | Ghost -> None)
  (* Module Strengthening *)
  | Module_type { mty_desc = Tmty_strengthen (_, _, mod_ident); _ } :: _ -> (
    (* Due to a current bug, there is no node for the module name after the `with`, so
       it's possible the cursor is on that instead of the `with`. *)
    match compare_cursor_to_loc mod_ident.loc with
    | Before ->
      Some
        { name = "Module strengthening";
          description =
            "Mark each type in this module type as equal to the corresponding \
             type in the given module";
          documentation =
            syntax_doc_url Oxcaml
              "miscellaneous-extensions/module-strengthening/";
          level = Advanced
        }
    | Inside | After | Ghost -> None)
  (* Local allocations *)
  | Expression { exp_desc = Texp_exclave _; _ } :: _ ->
    Some
      { name = "exclave_";
        description =
          "End the current region; the following code allocates in the outer \
           region";
        documentation = stack_allocation_url;
        level = Advanced
      }
  | Expression { exp_extra; exp_loc; _ } :: _
    when List.exists exp_extra ~f:(fun (extra, _, _) ->
             match extra with
             | Typedtree.Texp_stack -> true
             | _ -> false)
         && (* In this case, [exp_loc] differs from the location returned by
               [Browse_raw.node_merlin_loc] (which is whats used to determine [nodes]).
               The [Browse_raw.node_merlin_loc] one includes the stack_, whereas [exp_loc]
               doesn't. Since we already know that the cursor is in the
               [Browse_raw.node_merlin_loc] location (see the usage of [drop_until]
               above), we just need to check whether its in [exp_loc] to know whether it's
               on the [stack_] keyword. *)
         not (Loc_comparison_result.is_inside (compare_cursor_to_loc exp_loc))
    ->
    Some
      { name = "stack_";
        description = "Force the following allocation to be on stack.";
        documentation = stack_allocation_url;
        level = Advanced
      }
  (* Include functor *)
  | ( Include_description
        { incl_kind = Tincl_functor _ | Tincl_gen_functor _; _ }
    | Include_declaration
        { incl_kind = Tincl_functor _ | Tincl_gen_functor _; _ } )
    :: _ ->
    Some
      { name = "include functor";
        description =
          "Apply the functor to the current structure up to this point, and \
           include the result in the current structure";
        documentation =
          syntax_doc_url Oxcaml "miscellaneous-extensions/include-functor/";
        level = Advanced
      }
  | nodes ->
    (* The locations of attributes nodes only include the attribute name, not the payload.
       Additionally, the attribute node is not a parent of the payload node. But the
       attribute node will be a sibling of the payload. (Note that the bottom node might
       not be the payload but a node within the payload). So here we walk up the list of
       ancestors until we find one with an attribute as a child whose location includes
       the cursor position, at which point we can conclude the cursor is in the payload. *)
    List.find_map_opt nodes ~f:(fun ancestor ->
        let children =
          Browse_raw.fold_node
            (fun _ child acc -> child :: acc)
            Env.empty ancestor []
        in
        List.find_map_opt children ~f:(fun child ->
            match child with
            | Attribute attribute -> (
              match compare_cursor_to_loc attribute.attr_loc with
              | Inside -> get_doc_for_attribute attribute
              | Before | After | Ghost -> None)
            | _ -> None))

let get_syntax_doc cursor_loc node : syntax_info =
  let syntax_doc_url = syntax_doc_url Ocaml in
  match node with
  | (_, Type_kind _)
    :: (_, Type_declaration _)
    :: (_, With_constraint (Twith_typesubst _))
    :: _ ->
    Some
      { name = "Destructive substitution";
        description =
          "Behaves like normal signature constraints but removes the redefined \
           type or module from the signature.";
        documentation =
          syntax_doc_url
            "signaturesubstitution.html#ss:destructive-substitution";
        level = Simple
      }
  | (_, Type_kind _)
    :: (_, Type_declaration _)
    :: (_, Signature_item ({ sig_desc = Tsig_typesubst _; _ }, _))
    :: _ ->
    Some
      { name = "Local substitution";
        description =
          "Behaves like destructive substitution but is introduced during the \
           specification of the signature, and will apply to all the items \
           that follow.";
        documentation =
          syntax_doc_url "signaturesubstitution.html#ss:local-substitution";
        level = Simple
      }
  | (_, Module_type _)
    :: (_, Module_type _)
    :: ( _,
         Module_type_constraint
           (Tmodtype_explicit
             { mty_desc = Tmty_with (_, [ (_, _, Twith_modtype _) ]); _ }) )
    :: _ ->
    Some
      { name = "Module substitution";
        description =
          "Behaves like type substitutions but are useful to refine an \
           abstract module type in a signature into a concrete module type,";
        documentation =
          syntax_doc_url
            "signaturesubstitution.html#ss:module-type-substitution";
        level = Simple
      }
  | (_, Type_kind Ttype_open) :: (_, Type_declaration { typ_private; _ }) :: _
    ->
    let e_name = "Extensible Variant Type" in
    let e_description =
      "Can be extended with new variant constructors using `+=`."
    in
    let e_url = "extensiblevariants.html" in
    let name, description, url =
      match typ_private with
      | Public -> (e_name, e_description, e_url)
      | Private ->
        ( Format.sprintf "Private %s" e_name,
          Format.sprintf
            "%s. Prevents new constructors from being declared directly, but \
             allows extension constructors to be referred to in interfaces."
            e_description,
          "extensiblevariants.html#ss:private-extensible" )
    in
    Some
      { name;
        description;
        documentation = syntax_doc_url url;
        level = Advanced
      }
  | (_, Constructor_declaration _)
    :: (_, Type_kind (Ttype_variant _))
    :: (_, Type_declaration { typ_private; _ })
    :: _
  | _
    :: (_, Constructor_declaration _)
    :: (_, Type_kind (Ttype_variant _))
    :: (_, Type_declaration { typ_private; _ })
    :: _ ->
    let v_name = "Variant Type" in
    let v_description =
      "Represent's data that may take on multiple different forms."
    in
    let v_url = "typedecl.html#ss:typedefs" in
    let name, description, url =
      match typ_private with
      | Public -> (v_name, v_description, v_url)
      | Private ->
        ( Format.sprintf "Private %s" v_name,
          Format.sprintf
            "%s This type is private, values cannot be constructed directly \
             but can be de-structured as usual."
            v_description,
          "privatetypes.html#ss:private-types-variant" )
    in
    Some
      { name; description; documentation = syntax_doc_url url; level = Simple }
  | (_, Core_type _)
    :: (_, Core_type _)
    :: (_, Label_declaration _)
    :: (_, Type_kind (Ttype_record _))
    :: (_, Type_declaration { typ_private; _ })
    :: _ ->
    let r_name = "Record Type" in
    let r_description = "Defines variants with a fixed set of fields" in
    let r_url = "typedecl.html#ss:typedefs" in
    let name, description, url =
      match typ_private with
      | Public -> (r_name, r_description, r_url)
      | Private ->
        ( Format.sprintf "Private %s" r_name,
          Format.sprintf
            "%s This type is private, values cannot be constructed directly \
             but can be de-structured as usual."
            r_description,
          "privatetypes.html#ss:private-types-variant" )
    in
    Some
      { name; description; documentation = syntax_doc_url url; level = Simple }
  | (_, Type_kind (Ttype_variant _))
    :: (_, Type_declaration { typ_private = Public; _ })
    :: _ ->
    Some
      { name = "Empty Variant Type";
        description = "An empty variant type.";
        documentation = syntax_doc_url "emptyvariants.html";
        level = Advanced
      }
  | (_, Type_kind Ttype_abstract)
    :: (_, Type_declaration { typ_private = Public; typ_manifest = None; _ })
    :: _ ->
    Some
      { name = "Abstract Type";
        description =
          "Define variants with arbitrary data structures, including other \
           variants, records, and functions";
        documentation = syntax_doc_url "typedecl.html#ss:typedefs";
        level = Simple
      }
  | (_, Type_kind Ttype_abstract)
    :: (_, Type_declaration { typ_private = Private; _ })
    :: _ ->
    Some
      { name = "Private Type Abbreviation";
        description =
          "Declares a type that is distinct from its implementation type \
           `typexpr`.";
        documentation =
          syntax_doc_url "privatetypes.html#ss:private-types-abbrev";
        level = Simple
      }
  | (_, Expression _)
    :: (_, Expression _)
    :: (_, Value_binding _)
    :: (_, Structure_item ({ str_desc = Tstr_value (Recursive, _); _ }, _))
    :: _ ->
    Some
      { name = "Recursive value definition";
        description =
          "Supports a certain class of recursive definitions of non-functional \
           values.";
        documentation = syntax_doc_url "letrecvalues.html";
        level = Simple
      }
  | (_, Module_expr _) :: (_, Module_type { mty_desc = Tmty_typeof _; _ }) :: _
    ->
    Some
      { name = "Recovering module type";
        description =
          "Expands to the module type (signature or functor type) inferred for \
           the module expression `module-expr`. ";
        documentation = syntax_doc_url "moduletypeof.html";
        level = Simple
      }
  | (_, Module_expr _)
    :: (_, Module_expr _)
    :: (_, Module_binding _)
    :: (_, Structure_item ({ str_desc = Tstr_recmodule _; _ }, _))
    :: _ ->
    Some
      { name = "Recursive module";
        description =
          "A simultaneous definition of modules that can refer recursively to \
           each others.";
        documentation = syntax_doc_url "recursivemodules.html";
        level = Simple
      }
  | (_, Expression _)
    :: (_, Expression _)
    :: (_, Expression _)
    :: ( _,
         Value_binding
           { vb_expr =
               { exp_extra = [ (Texp_newtype (_, loc, _, _), _, _) ];
                 exp_loc;
                 _
               };
             _
           } )
    :: _ -> (
    let in_range =
      cursor_loc.Lexing.pos_cnum - 1 > exp_loc.loc_start.pos_cnum
      && cursor_loc.Lexing.pos_cnum <= loc.loc.loc_end.pos_cnum + 1
    in
    match in_range with
    | true ->
      Some
        { name = "Locally Abstract Type";
          description =
            "Type constructor which is considered abstract in the scope of the \
             sub-expression and replaced by a fresh type variable.";
          documentation = syntax_doc_url "locallyabstract.html";
          level = Simple
        }
    | false -> None)
  | (_, Module_expr _)
    :: (_, Module_expr _)
    :: (_, Expression { exp_desc = Texp_pack _; _ })
    :: _ ->
    Some
      { name = "First class module";
        description =
          "Converts a module (structure or functor) to a value of the core \
           language that encapsulates the module.";
        documentation = syntax_doc_url "firstclassmodules.html";
        level = Simple
      }
  | _ -> get_oxcaml_syntax_doc cursor_loc node
