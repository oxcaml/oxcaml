module Nlg = Diagnostic_nlg
open Nlg
module Dsl = Nlg.Dsl
module Step_mode = Mode.Reported_mode
module Side = Diagnostic_term.Side

type mismatch_step =
  { mode : Step_mode.t;
    pinpoint : Mode.Hint.pinpoint;
    kind : Mode.Reported_hint.t
  }

type term = Diagnostic_term.t

type fragment = term Nlg.fragment

type explanation_body = ((term, [`Dependent]) Nlg.node, fragment) Nonempty.t

let modality_word = Diagnostic_term.modality_word

let mode_const_word = Diagnostic_term.mode_const_word

let local_property =
  Diagnostic_term.mode_const_property (Comonadic Areality)
    Mode.Locality.Const.Local

let local = mode_const_word (Comonadic Areality) Mode.Locality.Const.Local

and once = mode_const_word (Comonadic Linearity) Mode.Linearity.Const.Once

and many = mode_const_word (Comonadic Linearity) Mode.Linearity.Const.Many

and portable =
  mode_const_word (Comonadic Portability) Mode.Portability.Const.Portable

and nonportable =
  mode_const_word (Comonadic Portability) Mode.Portability.Const.Nonportable

and unique = mode_const_word (Monadic Uniqueness) Mode.Uniqueness.Const.Unique

and aliased = mode_const_word (Monadic Uniqueness) Mode.Uniqueness.Const.Aliased

and uncontended =
  mode_const_word (Monadic Contention) Mode.Contention.Const.Uncontended

and shared = mode_const_word (Monadic Contention) Mode.Contention.Const.Shared

and contended =
  mode_const_word (Monadic Contention) Mode.Contention.Const.Contended

and read = mode_const_word (Monadic Visibility) Mode.Visibility.Const.Read

and write = mode_const_word (Monadic Visibility) Mode.Visibility.Const.Write

and read_write =
  mode_const_word (Monadic Visibility) Mode.Visibility.Const.Read_write

and static = mode_const_word (Monadic Staticity) Mode.Staticity.Static

and dynamic = mode_const_word (Monadic Staticity) Mode.Staticity.Dynamic

type argument_requirement =
  { callee : Mode.Hint.pinpoint;
    argument : Mode.Hint.pinpoint;
    parameter : Mode.Hint.parameter
  }

module Meaning = struct
  type capture_relation =
    | Closes_over
    | Used_inside

  type capture =
    { relation : capture_relation;
      details : Mode.Hint.closure_details;
      source_side : Side.t
    }

  type fact =
    | Mutable_read of Mode.Hint.mutable_part
    | Mutable_write of Mode.Hint.mutable_part
    | Lazy_allocated_on_heap
    | Lazy_forced
    | Module_allocated_on_heap
    | Unpacked_module
    | Legacy_construct of Mode.Hint.legacy
    | Toplevel_expression
    | Tailcall_function
    | Tailcall_argument
    | Function_return_default
    | Stack_allocated
    | Always_dynamic of Mode.Hint.always_dynamic
    | Has_branches
    | Layout_poly_instantiated
    | Borrowed
    | Region_escape of Mode.Hint.region
    | Quoted_computation
    | Spliced
    | Static_not_guaranteed of Compilation_unit.t option

  type shared_staticity =
    | Of_functor of Location.t
    | Of_functor_parameter of Location.t

  type reroute =
    | Mode_crossing
    | Partial_application_capture
    | Allocation of Mode.Hint.allocation
    | Contains of Mode.Hint.contains
    | Contained_by of Mode.Hint.is_contained_by
    | Shared_staticity of shared_staticity
    | Functor_application of Location.t
    | Functor_applied_at of Location.t

  type t =
    | Nothing_to_say
    | Unexplained
    | User_annotation of Mode.Hint.annotation_source
    | Capture of capture
    | Signature_argument of argument_requirement
    | Fact of fact
    | Reroute of reroute

  let reroute_of_allocation (allocation : Mode.Hint.allocation) =
    match allocation.txt with
    | Captured_by_partial_application -> Reroute Partial_application_capture
    | Unknown | Optional_argument | Function_coercion | Float_projection
    | Lpoly_captured_environment ->
      Reroute (Allocation allocation)

  let annotation_meaning mode
      ({ annotated_modes; contained_by } : Mode.Hint.annotation) =
    match List.assoc_opt (Step_mode.name mode) annotated_modes with
    | Some source -> User_annotation source
    | None -> (
      match contained_by with
      | Some containing -> Reroute (Contained_by containing)
      | None -> Unexplained)

  let interpret (s : mismatch_step) : t =
    match s.kind with
    | Morph Unknown | Morph Skip -> Nothing_to_say
    | Morph (Close_over (Comonadic, details)) ->
      Capture { relation = Closes_over; details; source_side = Actual }
    | Morph (Close_over (Monadic, details)) ->
      Capture { relation = Closes_over; details; source_side = Expected }
    | Morph (Is_closed_by (Comonadic, details)) ->
      Capture { relation = Used_inside; details; source_side = Expected }
    | Morph (Is_closed_by (Monadic, details)) ->
      Capture { relation = Used_inside; details; source_side = Actual }
    | Morph Crossing -> Reroute Mode_crossing
    | Morph (Functor_to_parameter loc) ->
      Reroute (Shared_staticity (Of_functor loc))
    | Morph (Parameter_to_functor loc) ->
      Reroute (Shared_staticity (Of_functor_parameter loc))
    | Morph (Functor_to_application loc) -> Reroute (Functor_application loc)
    | Morph (Application_to_functor loc) -> Reroute (Functor_applied_at loc)
    | Morph (Allocation_r alloc) | Morph (Allocation_l alloc) ->
      reroute_of_allocation alloc
    | Morph (Allocation _) -> Nothing_to_say
    | Morph (Contains_l (_, contains)) | Morph (Contains_r (_, contains)) ->
      Reroute (Contains contains)
    | Morph (Is_contained_by (_, c)) -> Reroute (Contained_by c)
    | Morph (Parameter_to_argument (_, { parameter; callee })) ->
      Signature_argument { parameter; callee; argument = s.pinpoint }
    | Morph (Argument_to_parameter (_, { parameter; argument })) ->
      Signature_argument { parameter; callee = s.pinpoint; argument }
    | Const Unknown -> Unexplained
    | Const (Annotation annotation) -> annotation_meaning s.mode annotation
    | Const Lazy_allocated_on_heap -> Fact Lazy_allocated_on_heap
    | Const (Legacy legacy) -> Fact (Legacy_construct legacy)
    | Const Toplevel_expression -> Fact Toplevel_expression
    | Const Tailcall_function -> Fact Tailcall_function
    | Const Tailcall_argument -> Fact Tailcall_argument
    | Const (Mutable_read part) -> Fact (Mutable_read part)
    | Const (Mutable_write part) -> Fact (Mutable_write part)
    | Const Lazy_forced -> Fact Lazy_forced
    | Const Function_return -> Fact Function_return_default
    | Const Stack_expression -> Fact Stack_allocated
    | Const Module_allocated_on_heap -> Fact Module_allocated_on_heap
    | Const Mod_unpack -> Fact Unpacked_module
    | Const (Always_dynamic x) -> Fact (Always_dynamic x)
    | Const Branching -> Fact Has_branches
    | Const Lpoly_inst -> Fact Layout_poly_instantiated
    | Const (Is_used_in closure) ->
      Capture
        { relation = Used_inside;
          details = { closure; closed = s.pinpoint };
          source_side = Expected
        }
    | Const (Borrowed (_, _)) -> Fact Borrowed
    | Const (Escape_region region) -> Fact (Region_escape region)
    | Const Quoted_computation -> Fact Quoted_computation
    | Const (Spliced _) -> Fact Spliced
    | Const (Contained_by c) -> Reroute (Contained_by c)
    | Const (Cmx_not_guaranteed unit) -> Fact (Static_not_guaranteed unit)
end

module Step = struct
  type t =
    { pinpoint : Mode.Hint.pinpoint;
      mode : Step_mode.t;
      says : Meaning.t
    }

  let of_chain (chain : mismatch_step list) : t list =
    List.filter_map
      (fun (s : mismatch_step) ->
        match Meaning.interpret s with
        | Nothing_to_say -> None
        | ( Unexplained | User_annotation _ | Capture _ | Signature_argument _
          | Fact _ | Reroute _ ) as says ->
          Some { pinpoint = s.pinpoint; mode = s.mode; says })
      chain

  let rec for_explanation (chain : t list) =
    match chain with
    | [] -> []
    | s :: rest ->
      let transparent =
        match s.says, rest with
        | (Reroute Mode_crossing | Reroute (Allocation _)), next :: _ ->
          Step_mode.equal s.mode next.mode
        | _ -> false
      in
      if transparent then for_explanation rest else s :: for_explanation rest
end

let human_desc : Mode.Hint.pinpoint_desc -> string = function
  | Unknown -> "this value"
  | Ident _ -> "this identifier"
  | Function -> "the anonymous function"
  | Parameter -> "the parameter"
  | Return -> "the function's return value"
  | Module -> "the module"
  | Functor -> "the functor"
  | Functor_parameter -> "the functor's parameter"
  | Structure -> "the structure"
  | Lazy -> "the lazy expression"
  | Quote -> "the quoted expression"
  | Allocation -> "the allocation"
  | Expression -> "the expression"
  | Effect_match -> "the pattern match with effect cases"
  | Effect_try -> "the try-with with effect cases"
  | Class -> "the class"
  | Object -> "the object"
  | Loop -> "the loop"
  | Letop -> "the letop"
  | Cases_result -> "the result of the cases"
  | Pattern -> "the pattern"
  | Structure_item _ -> "the structure item"

let noun_of_pinpoint ((loc, desc) : Mode.Hint.pinpoint) =
  match desc with
  | Ident { category; lid } ->
    let kind =
      match (category : Mode.Hint.lock_item) with
      | Value -> "value"
      | Module -> "module"
      | Class -> "class"
      | Constructor -> "constructor"
    in
    let name = Format_doc.asprintf "%a" Printtyp.Doc.longident lid in
    Noun.named ~loc ~kind name
  | Structure_item (_, id) -> Noun.named ~loc (Ident.name id)
  | Unknown | Function | Module | Functor | Functor_parameter | Parameter
  | Return | Structure | Lazy | Quote | Allocation | Expression | Effect_match
  | Effect_try | Class | Object | Loop | Letop | Cases_result | Pattern ->
    Noun.described ~loc (human_desc desc)

let noun_of_mode_chain (pinpoint : Mode.Hint.pinpoint) (chain : Step.t list) =
  let entity = noun_of_pinpoint pinpoint in
  match snd pinpoint, chain with
  | ( (Ident { category = Value; _ } | Structure_item (Value, _)),
      { says =
          Capture
            { relation = Closes_over;
              details = { closure = _, Function; _ };
              _
            };
        _
      }
      :: _ ) ->
    Noun.with_kind ~kind:"function" entity
  | _ -> entity

let describe_mode side mode =
  let property (description : Step_mode.described) =
    Diagnostic_term.mode_property description.semantic
  in
  Dsl.alternatives
    (match Step_mode.describe side mode with
    | [] -> [Diagnostic_term.mode_property mode]
    | first :: rest -> property first :: List.map property rest)

let mutable_part_noun (part : Mode.Hint.mutable_part) =
  match part with
  | Record_field field -> Noun.named ~kind:"mutable field" field
  | Array_elements -> Noun.common ~number:Plural "array elements"

let closes_over =
  Predicate.transitive ~singular:"closes over" ~plural:"close over"

let containing_text (containing : Mode.Hint.containing) =
  let with_modality noun = noun ^ ", with some modality" in
  match containing with
  | Tuple -> "as an element of the tuple"
  | Record (field, Modality) ->
    with_modality ("as field " ^ field ^ " of the record")
  | Array Modality -> with_modality "as an element of the array"
  | Constructor (name, Modality) -> with_modality ("via constructor " ^ name)
  | Structure (_, Modality) -> with_modality "in the structure"

let annotation_fragments ~mode_name ~mode ~subject:owner
    (source : Mode.Hint.annotation_source) : explanation_body =
  match source with
  | Written_modality written ->
    Nonempty.append
      [ Dsl.(
          reason
            (clause owner
               (annotated ~at:written.loc
                  (term (Diagnostic_term.Written_modality_term written.txt)))))
      ]
      (if String.equal mode_name written.txt
       then []
       else
         [ rule
             [ code mode_name;
               txt " is implied by the ";
               code written.txt;
               txt " modality" ] ])
  | Written_mode written ->
    [Dsl.(reason (clause owner (annotated_as ~at:written.loc mode)))]
  | Mutable_field field ->
    [ Dsl.(
        reason
          (clause
             (named ~loc:field.loc ~kind:"field" field.txt)
             (declared ~at:field.loc (Property.code "mutable"))));
      rule
        [ txt "mutable fields imply the ";
          term (Diagnostic_term.Written_modality_term mode_name);
          txt " modality by default" ] ]

let same_alloc_axis (Mode.With_locality.Axis.P left)
    (Mode.With_locality.Axis.P right) =
  Int.equal (Mode.With_locality.Axis.compare left right) 0

let explain_chain ?(necessity = Inherit) ~axis ~side ~subject:initial_subject
    chain : fragment list =
  let show_suggestions = Side.select side ~expected:true ~actual:false in
  let next_context ~side ~subject:owner (step : Step.t) rest =
    match step.says with
    | Capture { relation; details; source_side } ->
      let pinpoint =
        match relation with
        | Closes_over -> details.closed
        | Used_inside -> details.closure
      in
      source_side, noun_of_mode_chain pinpoint rest
    | Nothing_to_say | Unexplained -> side, owner
    | User_annotation _ | Signature_argument _ | Fact _ | Reroute _ ->
      let subject =
        match rest with
        | [] -> owner
        | (next : Step.t) :: _ ->
          let same_location =
            Structured_diagnostic.Location_key.equal
              (Structured_diagnostic.Location_key.of_location
                 (fst step.pinpoint))
              (Structured_diagnostic.Location_key.of_location
                 (fst next.pinpoint))
          in
          if same_location then owner else noun_of_pinpoint next.pinpoint
      in
      side, subject
  in
  let rec explain ~side ~subject:(owner : subject) (chain : Step.t list) =
    match chain with
    | [] -> [], None
    | { says = Nothing_to_say | Unexplained; _ } :: rest ->
      explain ~side ~subject:owner rest
    | s :: rest -> (
      let subj = mention ~case:Subject owner in
      let subject_possessive = mention ~case:Possessive owner in
      let mode =
        describe_mode
          (Side.select side ~expected:`Expected ~actual:`Actual)
          s.mode
      in
      let continuation =
        lazy
          (let side, subject = next_context ~side ~subject:owner s rest in
           explain ~side ~subject rest)
      in
      let continue () = fst (Lazy.force continuation) in
      let mutable_access ~writing part : explanation_body =
        let accessed = Noun.possessive owner (mutable_part_noun part) in
        let access = if writing then "being written" else "being read" in
        Nonempty.append
          [Dsl.(reason (clause accessed (is (property access))))]
          (match axis with
          | Mode.With_locality.Axis.P (Monadic (Contention | Visibility)) ->
            let part, owner =
              match (part : Mode.Hint.mutable_part) with
              | Record_field _ -> "a mutable field", "the value"
              | Array_elements -> "mutable array elements", "the array"
            in
            let required =
              match axis, writing with
              | Mode.With_locality.Axis.P (Monadic Contention), true ->
                [uncontended]
              | Mode.With_locality.Axis.P (Monadic Contention), false ->
                [shared; txt " or "; uncontended]
              | _, true -> [write; txt " or "; read_write]
              | _, false -> [read; txt " or "; read_write]
            in
            [ rule
                (txt
                   ((if writing then "writing " else "reading ")
                   ^ part ^ " requires " ^ owner ^ " to be ")
                :: required) ]
          | _ -> [])
      in
      let fragments : explanation_body option =
        match s.says with
        | Nothing_to_say | Unexplained -> None
        | User_annotation source ->
          Some
            (annotation_fragments ~mode_name:(Step_mode.name s.mode) ~mode
               ~subject:owner source)
        | Capture { relation; details; source_side } ->
          Some
            (let pinpoint, relation_predicate =
               match relation with
               | Closes_over -> details.closed, closes_over
               | Used_inside -> details.closure, Dsl.used_inside
             in
             let captured_entity = noun_of_mode_chain pinpoint rest in
             Nonempty.append
               [ Dsl.(
                   reason
                     (clause owner
                        (relation_predicate
                           (source (noun_of_pinpoint pinpoint)))))
                 |> with_children
                      (match rest with
                      | [] -> []
                      | next :: _ ->
                        let mode =
                          describe_mode
                            (Side.select source_side ~expected:`Expected
                               ~actual:`Actual)
                            next.mode
                        in
                        let predicate =
                          match relation, source_side with
                          | _, Actual -> Dsl.is mode
                          | Closes_over, Expected -> Dsl.used_as mode
                          | Used_inside, Expected -> Dsl.(is (expected mode))
                        in
                        [ Dsl.(and_ (clause captured_entity predicate))
                          |> with_children (continue ()) ]) ]
               (let step_on wanted (step : Step.t) =
                  match Mode.reported_mode_as_alloc_atom step.mode with
                  | None -> false
                  | Some (Mode.With_locality.Atom (axis, _)) ->
                    same_alloc_axis (Mode.With_locality.Axis.P axis) wanted
                in
                let crosses ~source ~target =
                  step_on source s
                  &&
                  match rest with
                  | [] -> false
                  | next :: _ -> step_on target next
                in
                let portability =
                  Mode.With_locality.Axis.P (Comonadic Portability)
                in
                let contention =
                  Mode.With_locality.Axis.P (Monadic Contention)
                in
                if
                  same_alloc_axis axis portability
                  && crosses ~source:portability ~target:contention
                then
                  [ rule
                      [ txt "a function that closes over ";
                        uncontended;
                        txt " data";
                        copula;
                        txt " ";
                        nonportable ] ]
                else if
                  same_alloc_axis axis contention
                  && crosses ~source:contention ~target:portability
                then
                  [ rule
                      [ txt "values used inside a ";
                        portable;
                        txt " function";
                        copula_agreeing Plural;
                        txt " ";
                        contended ] ]
                else []))
        | Signature_argument
            { callee;
              argument;
              parameter = { label; index_in_callee_arrow_type }
            } ->
          Some
            (let callee = Noun.short (noun_of_pinpoint callee) in
             let argument = Noun.short (noun_of_pinpoint argument) in
             let argument_mention =
               match Noun.identifier argument with
               | Some _ -> [txt ", "; mention ~case:Subject argument; txt ","]
               | None -> []
             in
             let position =
               match (label : Mode.Hint.argument_label) with
               | Labelled label | Position label ->
                 [code ("~" ^ label); txt " argument"]
               | Optional label -> [code ("?" ^ label); txt " argument"]
               | Unlabelled ->
                 [txt (ordinal (index_in_callee_arrow_type + 1) ^ " argument")]
             in
             [ reason
                 (mention ~case:Subject callee
                 :: (txt " requires "
                     :: mention ~case:Possessive callee
                     :: txt " " :: position
                    @ argument_mention
                    @ [txt " to be "; Verbatim.phrase mode]))
               |> with_children (continue ()) ])
        | Fact (Mutable_read part) -> Some (mutable_access ~writing:false part)
        | Fact (Mutable_write part) -> Some (mutable_access ~writing:true part)
        | Fact Lazy_allocated_on_heap ->
          Some
            [ reason
                [subj; copula; txt " a lazy expression allocated on the heap"]
            ]
        | Fact Module_allocated_on_heap ->
          Some [reason [subj; copula; txt " a module allocated on the heap"]]
        | Fact Unpacked_module ->
          Some [reason [txt "unpacked first-class modules are always dynamic"]]
        | Fact (Legacy_construct legacy) ->
          Some
            (let what =
               match (legacy : Mode.Hint.legacy) with
               | Toplevel -> "a top-level definition"
               | Compilation_unit -> "a compilation unit"
               | Class -> "a class"
               | Quoted -> "a quoted expression's result"
             in
             [ reason
                 [ subj;
                   copula;
                   txt (" " ^ what ^ ", which always has the legacy modes") ] ])
        | Fact Layout_poly_instantiated ->
          Some
            [ reason
                [subj; copula; txt " layout-polymorphic and instantiated here"]
            ]
        | Fact Lazy_forced ->
          Some
            [ Dsl.(
                reason
                  (clause owner (is (property "a lazy value being forced")))) ]
        | Fact Toplevel_expression ->
          Some
            [ Dsl.(
                reason (clause owner (is (property "a top-level expression"))))
            ]
        | Fact Tailcall_function ->
          Some
            [ Dsl.(
                reason
                  (clause owner (is (property "the function of a tail call"))))
            ]
        | Fact Tailcall_argument ->
          Some
            [ Dsl.(
                reason
                  (clause owner (is (property "an argument of a tail call"))))
            ]
        | Fact Function_return_default ->
          Some
            (Nonempty.append
               [ Dsl.(
                   reason
                     (clause owner (is (property "returned from a function"))))
               ]
               (if show_suggestions
                then
                  [ suggestion
                      [ txt "use ";
                        code "exclave_";
                        txt " to return a ";
                        local;
                        txt " value" ] ]
                else []))
        | Fact Stack_allocated ->
          Some [reason [subj; copula; txt " allocated with "; code "stack_"]]
        | Fact (Always_dynamic x) ->
          Some
            (let what =
               match (x : Mode.Hint.always_dynamic) with
               | Application -> "function applications"
               | Try_with -> "try-with clauses"
               | Generative_functor -> "generative functor applications"
             in
             [reason [txt (what ^ " are always dynamic")]])
        | Fact Has_branches -> Some [reason [subj; txt " has branches"]]
        | Fact Borrowed ->
          Some [Dsl.(reason (clause owner (is (property "borrowed"))))]
        | Fact (Region_escape (loc, Borrow)) ->
          Some
            (let escape = txt " escapes a borrow region" in
             Nonempty.append
               [ reason
                   [ subj;
                     (if Location.is_none loc
                      then escape
                      else ref_source loc [escape]) ] ]
               (match axis with
               | Mode.With_locality.Axis.P (Comonadic Areality) ->
                 [rule [local; txt " values cannot escape their region"]]
               | _ -> []))
        | Fact Quoted_computation ->
          Some
            [ Dsl.(
                reason
                  (clause owner (is (property "the quote of a computation"))))
            ]
        | Fact Spliced ->
          Some [Dsl.(reason (clause owner (is (property "spliced"))))]
        | Fact (Static_not_guaranteed (Some unit)) ->
          Some
            [ reason
                [ code (Compilation_unit.name_as_string unit);
                  txt
                    " is neither a core library nor the current library, and \
                     only those can be ";
                  static ] ]
        | Fact (Static_not_guaranteed None) ->
          Some [reason [txt "parameter modules are always "; dynamic]]
        | Reroute Mode_crossing ->
          Some
            [ reason
                [ subj;
                  txt " crosses modes based on ";
                  subject_possessive;
                  txt " type" ]
              |> with_children (continue ()) ]
        | Reroute Partial_application_capture ->
          Some
            [ reason [subj; copula; txt " captured by a partial application"]
              |> with_children (continue ()) ]
        | Reroute (Allocation { txt = desc; loc }) ->
          Some
            (let located words =
               if Location.is_none loc
               then Nonempty.to_list words
               else [ref_source loc words]
             in
             let specific : term Phrase.t =
               match (desc : Mode.Hint.allocation_desc) with
               | Unknown -> [copula; txt " an allocation"]
               | Optional_argument ->
                 [copula; txt " boxed as an optional argument"]
               | Function_coercion -> [copula; txt " partially applied"]
               | Float_projection -> [copula; txt " a float-record projection"]
               | Lpoly_captured_environment ->
                 [txt " captures a layout-polymorphic environment"]
               | Captured_by_partial_application ->
                 [copula; txt " captured by a partial application"]
             in
             [reason (subj :: located specific) |> with_children (continue ())])
        | Reroute (Contains { containing; contained }) ->
          Some
            (let contained = noun_of_pinpoint contained in
             [ reason
                 [ subj;
                   txt " contains ";
                   located_mention contained;
                   txt (" (" ^ containing_text containing ^ ")") ]
               |> with_children (continue ()) ])
        | Reroute (Contained_by { containing; container }) ->
          Some
            (let container = noun_of_pinpoint container in
             [ reason
                 [ subj;
                   copula;
                   txt " contained in ";
                   located_mention container;
                   txt (" (" ^ containing_text containing ^ ")") ]
               |> with_children (continue ()) ])
        | Reroute (Shared_staticity shared) ->
          Some
            (let related =
               match shared with
               | Of_functor loc -> Noun.described ~loc "the functor"
               | Of_functor_parameter loc ->
                 Noun.described ~loc "the functor parameter"
             in
             [ reason
                 [subj; txt " shares the staticity of "; located_mention related]
               |> with_children (continue ()) ])
        | Reroute (Functor_application loc) ->
          Some
            (let applied = Noun.described ~loc "the functor" in
             [ reason
                 [ subj;
                   copula;
                   txt " an application of ";
                   located_mention applied ]
               |> with_children (continue ()) ])
        | Reroute (Functor_applied_at loc) ->
          Some
            (let application = Noun.described ~loc "this application" in
             [ reason
                 [subj; copula; txt " applied at "; located_mention application]
               |> with_children (continue ()) ])
      in
      match fragments with
      | None -> Lazy.force continuation
      | Some (first :: rest as body) ->
        let is_cause =
          match s.says with
          | User_annotation _ -> true
          | _ -> Nlg.is_rule first || List.exists Nlg.is_rule rest
        in
        let fragment : (term, [`Explanation]) Nlg.node =
          explanation ~necessity body
        in
        let cause =
          if is_cause
          then Some fragment
          else if Lazy.is_val continuation
          then snd (Lazy.force continuation)
          else None
        in
        [(fragment :> fragment)], cause)
  in
  let fragments, cause = explain ~side ~subject:initial_subject chain in
  match side, cause with
  | Side.Actual, Some cause -> Nlg.focus ~on:cause fragments
  | Side.Expected, _ | Side.Actual, None -> fragments

let plan_partial_application_hint ~(axis : Mode.With_locality.Axis.packed)
    (result_type : Types.type_expr) : fragment list =
  match axis with
  | Mode.With_locality.Axis.P (Mode.With_locality.Axis.Comonadic Areality) ->
    begin
    let rec non_local_arity sure n ty =
      match Types.get_desc ty with
      | Types.Tarrow ((_, _, res_mode), _, res_ty, _) ->
        begin match
          Mode.Locality.Guts.check_const
            (Mode.With_locality.proj_comonadic Areality res_mode)
        with
        | Some Global -> Some (n + 1, true)
        | Some Local -> non_local_arity sure (n + 1) res_ty
        | None -> non_local_arity false (n + 1) res_ty
        end
      | _ -> if n = 0 then None else Some (n, sure)
    in
    match non_local_arity true 0 result_type with
    | None -> []
    | Some (n, sure) ->
      let arguments = if n = 1 then "argument" else "arguments" in
      let qualifier = if sure then "will" else "may" in
      [ rule [txt "this is a partial application"];
        suggestion
          [ txt
              ("adding " ^ string_of_int n ^ " more " ^ arguments ^ " "
             ^ qualifier ^ " make the value non-local") ] ]
    end
  | Mode.With_locality.Axis.P _ -> []

type actuality_note = Arguments_do_not_cross

type extra_rules =
  { for_actual : fragment list;
    for_expected : fragment list
  }

let no_extra_rules = { for_actual = []; for_expected = [] }

type axis_input =
  { axis : Mode.With_locality.Axis.packed;
    actual : mismatch_step list;
    expected : mismatch_step list;
    actual_description : term Property.t;
    expected_description : term Property.t;
    actual_loosened : bool;
    expected_loosened : bool
  }

let mode_comparison ~loosened ~(side : Side.t) mode =
  if loosened
  then Side.select side ~expected:Dsl.stronger_than ~actual:Dsl.weaker_than mode
  else mode

let signature_reason ~axis ~subject:owner
    (declaration : Types.value_description option) : fragment list =
  match declaration with
  | None -> []
  | Some declaration ->
    let modalities = declaration.val_modalities in
    let constant =
      if Mode.Modality.is_undefined modalities
      then None
      else Mode.Modality.to_const_opt modalities
    in
    begin match constant with
    | None -> []
    | Some modalities -> (
      let (Mode.Modality.Axis.P axis) =
        Mode.Modality.Axis.of_value
          (Mode.Const.Axis.with_locality_as_regionality axis)
      in
      let modality = Mode.Modality.Const.proj axis modalities in
      if Mode.Modality.Per_axis.is_id axis modality
      then []
      else
        match Mode.Modality.Const.annotation axis modalities with
        | Some source ->
          let mode_name =
            Format_doc.asprintf "%a"
              (Mode.Modality.Per_axis.print axis)
              modality
          in
          [ explanation ~necessity:Unnecessary
              (annotation_fragments ~mode_name ~mode:(Property.code mode_name)
                 ~subject:owner source) ]
        | None ->
          [ explanation ~necessity:Unnecessary
              [ reason
                  [ mention ~case:Possessive owner;
                    txt " signature requires ";
                    ref_source declaration.val_loc
                      [modality_word (Mode.Modality.Atom (axis, modality))] ] ]
          ])
    end

let plan_axis ~extra_rules ~actuality_note ~subject_override
    ~expected_declaration ~error_loc
    ({ axis;
       actual;
       expected;
       actual_description;
       expected_description;
       actual_loosened;
       expected_loosened
     } :
      axis_input) : ((term, [`Statement]) Nlg.node, fragment) Nonempty.t =
  let actual = Step.for_explanation (Step.of_chain actual) in
  let expected = Step.for_explanation (Step.of_chain expected) in
  let subject =
    match (subject_override : subject option) with
    | Some subject -> subject
    | None -> (
      match actual with
      | (s : Step.t) :: _ -> noun_of_pinpoint s.pinpoint
      | [] -> Noun.described ~loc:error_loc "this value")
  in
  let actuality_explanation =
    match actuality_note with
    | None -> []
    | Some Arguments_do_not_cross ->
      let axis_name =
        match axis with
        | Mode.With_locality.Axis.P axis ->
          Format_doc.asprintf "%a" Mode.With_locality.Axis.print axis
      in
      [ elaborate
          [ txt "the argument types of ";
            mention ~case:Subject subject;
            txt (" do not all cross " ^ axis_name) ] ]
  in
  let signature_reason =
    if
      List.exists
        (fun (step : Step.t) ->
          match step.says with
          | User_annotation (Written_modality _ | Mutable_field _) -> true
          | _ -> false)
        expected
    then []
    else signature_reason ~axis ~subject expected_declaration
  in
  let expected_mode =
    mode_comparison ~loosened:expected_loosened ~side:Expected
      expected_description
  in
  let actual_mode =
    mode_comparison ~loosened:actual_loosened ~side:Actual actual_description
  in
  let expected_fragment : (term, [`Statement]) Nlg.node =
    Dsl.(state (clause subject (is (expected expected_mode))))
    |> with_children
         (explain_chain ~necessity:Unnecessary ~axis ~side:Expected ~subject
            expected
         @ signature_reason @ extra_rules.for_expected)
  in
  let actual_fragment =
    Dsl.(but (clause subject (is actual_mode)))
    |> with_children
         (explain_chain ~axis ~side:Actual ~subject actual
         @ actuality_explanation @ extra_rules.for_actual)
  in
  [expected_fragment; actual_fragment]

let fold_step ~mode ~pinpoint ~hint chain =
  { mode; pinpoint; kind = hint } :: chain

let prepare_axis
    ({ actual;
       expected;
       actual_mode;
       expected_mode;
       actual_loosened;
       expected_loosened
     } :
      mismatch_step list Mode.folded_axis) =
  match Mode.reported_mode_as_alloc_atom actual_mode with
  | None -> None
  | Some (Mode.With_locality.Atom (axis, _)) ->
    Some
      { axis = Mode.With_locality.Axis.P axis;
        actual;
        expected;
        actual_description = describe_mode `Actual actual_mode;
        expected_description = describe_mode `Expected expected_mode;
        actual_loosened;
        expected_loosened
      }

type expression_error =
  | Submode_failed of
      { error : Mode.With_regionality.error;
        context : Typecore.submode_reason
      }
  | Curried_application_complete of
      { label : Typedtree.arg_label;
        error : Mode.With_locality.error;
        part : [`Prefix | `Single_arg | `Entire_apply]
      }
  | Function_mode_mismatch of
      { part : Typecore.mode_mismatch_kind;
        direction : Mode.equate_step;
        error : Mode.With_locality.error
      }
  | Uncurried_function_escapes_comonadic of Mode.With_locality.Comonadic.error
  | Overwrite_of_invalid_term
  | Block_index_modality_mismatch of
      { mutable_elements : bool;
        error : Mode.Modality.equate_error
      }
  | Exclave_in_nontail_position
  | Exclave_returns_not_local
  | Tail_call_local_returning
  | Always_heap_allocation of Typecore.always_heap_allocation
  | Always_static_allocation of Typecore.always_static_allocation
  | Not_allocation

type error =
  | Expression_error of
      { loc : Location.t;
        error : expression_error
      }
  | Constructor_submode_failed of
      { loc : Location.t;
        error : Mode.With_regionality.error
      }
  | Local_value_used_in_exclave of
      { loc : Location.t;
        description : Mode.Hint.pinpoint_desc
      }
  | Mutable_value_used_in_closure of
      { loc : Location.t;
        pinpoint : Mode.Hint.pinpoint
      }
  | Unique_use_during_borrowing of
      Uniqueness_analysis.Usage.unique_use_during_borrowing_error
  | Uniqueness_error of Uniqueness_analysis.error
  | Folded_mismatch of mismatch_step list Mode.folded_axis list

type modality_subject =
  | Modality_item of string
  | Modality_field of string
  | Modality_constructor_arg of
      { constructor : string;
        index : int
      }

type modality_side =
  { atom : Mode.Modality.atom option;
    loc : Location.t option
  }

type modality_requirement =
  | Exact_match
  | At_least_as_strong

type modality_input =
  { axis : Mode.With_regionality.Axis.packed;
    subject : modality_subject;
    expected : modality_side;
    actual : modality_side;
    requirement : modality_requirement
  }

let modality_fragment ~(sides : Diagnostic_term.sides) (input : modality_input)
    : fragment =
  let axis_name =
    match input.axis with
    | Mode.With_regionality.Axis.P ax ->
      Format_doc.asprintf "%a" Mode.With_regionality.Axis.print ax
  in
  let subject : subject =
    let span =
      match input.actual.loc, input.expected.loc with
      | Some l, _ | None, Some l -> Some l
      | None, None -> None
    in
    match input.subject with
    | Modality_item name -> subject ?span [Phrase.Code name]
    | Modality_field name ->
      subject ?span [Phrase.Text "the field "; Phrase.Code name]
    | Modality_constructor_arg { constructor; index } ->
      subject ?span
        [ Phrase.Text ("the " ^ ordinal index ^ " argument of ");
          Phrase.Code constructor ]
  in
  let side ~name ({ atom; loc } : modality_side) : term Phrase.segment list =
    let words : term Phrase.t =
      match atom with
      | Some atom ->
        copula :: txt " " :: modality_word atom :: txt " in "
        :: Nonempty.to_list name
      | None ->
        txt (" has no " ^ axis_name ^ " modality")
        :: txt " in " :: Nonempty.to_list name
    in
    match loc with
    | None -> Nonempty.to_list words
    | Some l -> [ref_source l words]
  in
  let header : term Phrase.t =
    [ txt "the declarations of ";
      mention ~case:Subject subject;
      txt (" disagree on " ^ axis_name) ]
  in
  let expected_line : term Phrase.t =
    mention ~case:Subject subject
    :: side ~name:sides.Diagnostic_term.expected_name input.expected
  in
  let actual_line : term Phrase.t =
    mention ~case:Subject subject
    :: side ~name:sides.Diagnostic_term.actual_name input.actual
  in
  let rules =
    match input.requirement with
    | At_least_as_strong -> []
    | Exact_match ->
      [ rule
          [ txt
              "field and constructor-argument modalities must match exactly on \
               both sides" ] ]
  in
  state header
  |> with_children
       [elaborate expected_line; state actual_line |> with_children rules]

let mode_fragments ~error_loc ?extra_rules ?actuality_note ?subject_override
    ?expected_declaration (axes : mismatch_step list Mode.folded_axis list) :
    fragment list =
  List.filter_map prepare_axis axes
  |> List.map (fun (input : axis_input) ->
      let extra_rules =
        match extra_rules with
        | None -> no_extra_rules
        | Some rules -> rules input.axis
      in
      block
        (plan_axis ~extra_rules ~actuality_note ~subject_override
           ~expected_declaration ~error_loc input))

let mode_error_fragments ~error_loc ?expected_declaration pinpoint error =
  mode_fragments ~error_loc ?expected_declaration
    (Mode.With_regionality.fold_error ~init:[] ~step:fold_step pinpoint error)

let describe_usage usage =
  let open Uniqueness_analysis.Usage in
  let { action; context } = view usage in
  let action =
    match action with
    | Use -> "used"
    | Borrow -> "borrowed"
    | Read -> "read from"
    | Write -> "written to"
  in
  match context with
  | Direct -> action
  | In_pattern Lazy -> action ^ " in a lazy pattern"
  | In_pattern Array -> action ^ " in an array pattern"
  | In_pattern Constant -> action ^ " in a constant pattern"
  | In_closure_that_might_be_called_later ->
    action ^ " in a closure that might be called later"
  | While_being_borrowed -> action ^ " while being borrowed"

let diagnose ~error_loc = function
  | Expression_error { loc; error = err } -> begin
    let fold_value error =
      Mode.With_regionality.fold_error ~init:[] ~step:fold_step
        (loc, Mode.Hint.Expression)
        error
    in
    let fold_alloc error =
      Mode.With_locality.fold_error ~init:[] ~step:fold_step
        (loc, Mode.Hint.Expression)
        error
    in
    match err with
    | Submode_failed { error = e; context } ->
      let extra_rules, actuality_note =
        match (context : Typecore.submode_reason) with
        | Constructor _ ->
          ( (fun _axis ->
              { no_extra_rules with
                for_actual =
                  [ rule
                      [ txt
                          "using a constructor across a mode boundary requires \
                           all its argument types to mode-cross" ] ]
              }),
            Some Arguments_do_not_cross )
        | Application result_type ->
          ( (fun axis ->
              { no_extra_rules with
                for_expected = plan_partial_application_hint ~axis result_type
              }),
            None )
        | Other -> (fun _axis -> no_extra_rules), None
      in
      mode_fragments ~error_loc ~extra_rules ?actuality_note (fold_value e)
    | Curried_application_complete { label = lbl; error = e; part } ->
      let argument_words =
        match (lbl : Typedtree.arg_label) with
        | Nolabel -> [Phrase.Text "this argument"]
        | Labelled s | Position s -> [Phrase.Code ("~" ^ s)]
        | Optional s -> [Phrase.Code ("?" ^ s)]
      in
      let subject_override : subject option =
        match part with
        | `Prefix -> None
        | `Single_arg ->
          Some
            (subject ~span:loc
               [Phrase.Text "the application up to this argument"])
        | `Entire_apply ->
          Some
            (subject ~span:loc
               (Phrase.Text "the application up to " :: argument_words))
      in
      let restricted_word (axis : Mode.With_locality.Axis.packed) =
        match axis with
        | Mode.With_locality.Axis.P
            (Mode.With_locality.Axis.Comonadic Areality) ->
          Some local
        | Mode.With_locality.Axis.P
            (Mode.With_locality.Axis.Comonadic Linearity) ->
          Some once
        | Mode.With_locality.Axis.P _ -> None
      in
      let suggestion_phrases : term Phrase.t list =
        match part with
        | `Prefix -> [[txt "try wrapping the marked application in parentheses"]]
        | `Single_arg ->
          [ [txt "try splitting the application in two"];
            [ txt
                "the arguments after this one in the function's type should be \
                 applied separately" ] ]
        | `Entire_apply ->
          [ [txt "try splitting the application in two"];
            txt "the arguments after "
            :: (List.map Nlg.word argument_words
               @ [txt " in the function's type should be applied separately"])
          ]
      in
      let extra_rules axis =
        match restricted_word axis with
        | None -> no_extra_rules
        | Some word ->
          { no_extra_rules with
            for_expected =
              rule
                [ txt "when passing or calling ";
                  word;
                  txt
                    " values, extra arguments are passed in a separate \
                     application" ]
              :: List.map suggestion suggestion_phrases
          }
      in
      mode_fragments ~error_loc ~extra_rules ?subject_override (fold_alloc e)
    | Function_mode_mismatch { part; direction = step; error = e } ->
      let subject_override : subject option =
        match (part : Typecore.mode_mismatch_kind) with
        | Parameter ->
          Some (subject ~span:loc [Phrase.Text "this function's parameter"])
        | Return ->
          Some (subject ~span:loc [Phrase.Text "this function's return value"])
      in
      let axes = fold_alloc e in
      let axes =
        match (step : Mode.equate_step) with
        | Left_le_right -> axes
        | Right_le_left ->
          List.map
            (fun (a : mismatch_step list Mode.folded_axis) ->
              { Mode.actual = a.expected;
                expected = a.actual;
                actual_mode = a.expected_mode;
                expected_mode = a.actual_mode;
                actual_loosened = a.expected_loosened;
                expected_loosened = a.actual_loosened
              })
            axes
      in
      mode_fragments ~error_loc ?subject_override axes
    | Uncurried_function_escapes_comonadic e ->
      let subject_override : subject option =
        Some
          (subject ~span:loc
             [Phrase.Text "this function when partially applied"])
      in
      let extra_rules _axis =
        { no_extra_rules with
          for_actual =
            [ rule
                [ txt
                    "partially applying a function closes over the arguments \
                     given so far" ] ]
        }
      in
      mode_fragments ~error_loc ~extra_rules ?subject_override
        (fold_alloc (Mode.With_locality.Comonadic e))
    | Overwrite_of_invalid_term ->
      [ block
          [ state [ref_source loc [txt "this term cannot be overwritten"]];
            but
              [ txt
                  "overwriting works only on tuples, constructors and boxed \
                   records" ]
            |> with_children
                 [ rule
                     [ code "overwrite_";
                       txt
                         " reuses an existing block, so the value must be one \
                          that occupies a block of its own" ] ] ] ]
    | Block_index_modality_mismatch { mutable_elements = mut; error = err } ->
      let _step, Mode.Modality.Error (ax, { left; right = _ }) = err in
      let axis_name =
        match Mode.Modality.Axis.to_value (Mode.Modality.Axis.P ax) with
        | Mode.With_regionality.Axis.P vax ->
          Format_doc.asprintf "%a" Mode.With_regionality.Axis.print vax
      in
      let actual_words =
        if Mode.Modality.Per_axis.is_id ax left
        then [txt ("no modality on the " ^ axis_name ^ " axis")]
        else [txt "the modality "; modality_word (Mode.Modality.Atom (ax, left))]
      in
      [ block
          [ state
              [ ref_source loc
                  (txt "this block index reaches a field with " :: actual_words)
              ];
            but
              [ txt
                  ("a block index over "
                  ^ (match mut with true -> "mutable" | false -> "immutable")
                  ^ " elements requires the modalities implied by its \
                     declaration, and no others") ]
            |> with_children
                 [ rule
                     [ txt
                         "this is a current limitation: the block-index \
                          primitives are typed with one fixed modality and \
                          cannot express others yet" ];
                   suggestion
                     [ txt
                         "remove the modality from the field, or read the \
                          field directly instead of taking an index" ] ] ] ]
    | Exclave_in_nontail_position ->
      let subject = Noun.described ~loc "this expression" in
      [ block
          [ state
              [ ref_source loc
                  (mention ~case:Subject subject
                  :: [copula; txt " not in tail position"]) ];
            but
              [ code "exclave_";
                txt " must be the last thing the enclosing region evaluates" ]
            |> with_children
                 [ rule
                     [ code "exclave_";
                       txt
                         " puts a value in the caller's region, so it can only \
                          appear where the current region is about to end" ] ]
          ] ]
    | Exclave_returns_not_local ->
      let subject = Noun.described ~loc "this expression" in
      [ block
          [ state [located_mention subject; copula; txt " "; local]
            |> with_children
                 [ reason
                     [ code "exclave_";
                       txt " makes ";
                       mention ~case:Subject subject;
                       txt " ";
                       local ] ];
            but
              [ txt "the enclosing function is not declared to return a ";
                local;
                txt " value" ]
            |> with_children
                 [ rule
                     [ txt "a function containing ";
                       code "exclave_";
                       txt
                         " allocates into its caller's region, so it must \
                          itself be ";
                       local;
                       txt "-returning" ];
                   suggestion
                     [ txt "annotate the function's result as ";
                       local;
                       txt ", or drop the ";
                       code "exclave_" ] ] ] ]
    | Tail_call_local_returning ->
      let subject = Noun.described ~loc "this call" in
      [ block
          [ state
              [ ref_source loc
                  (mention ~case:Subject subject
                  :: [txt " returns a "; local; txt " value"]) ];
            but
              [ mention ~case:Subject subject;
                copula;
                txt " in the tail position of a function that is not ";
                local;
                txt "-returning" ]
            |> with_children
                 [ rule
                     [ txt
                         "a tail call hands its result straight to the caller, \
                          so a ";
                       local;
                       txt
                         "-returning call can only sit in the tail of a \
                          local-returning function" ];
                   suggestion
                     [ txt "bind the result first, as in ";
                       code "let r = ... in r";
                       txt ", so the call is no longer in tail position" ] ] ]
      ]
    | Always_heap_allocation kind ->
      let what =
        match (kind : Typecore.always_heap_allocation) with
        | Lazy -> "a lazy expression"
        | Module -> "a module"
        | Object -> "an object"
        | List_comprehension -> "a list comprehension"
        | Array_comprehension -> "an array comprehension"
      in
      [ block
          [ state
              [ ref_source loc
                  [txt ("the compiler cannot stack-allocate " ^ what ^ " yet")]
              ]
            |> with_children
                 [ rule
                     [ txt
                         "this is a current limitation, not a rule of the \
                          language" ];
                   suggestion
                     [ txt "drop the ";
                       code "stack_";
                       txt " and let this allocate on the heap" ] ] ] ]
    | Always_static_allocation kind ->
      let what =
        match (kind : Typecore.always_static_allocation) with
        | Constant -> "a literal"
        | Src_pos -> "a source position literal"
        | Unboxed_unit -> "an unboxed unit literal"
        | Unboxed_bool -> "an unboxed boolean literal"
      in
      [ block
          [ state [ref_source loc [txt (what ^ " is not allocated at runtime")]];
            but
              [code "stack_"; txt " must be applied to something that allocates"]
            |> with_children
                 [ rule
                     [ code "stack_";
                       txt
                         " chooses where an allocation happens, and this value \
                          needs no allocation to choose from" ];
                   suggestion [txt "remove the "; code "stack_"] ] ] ]
    | Not_allocation ->
      let subject = Noun.described ~loc "this expression" in
      [ block
          [ state
              [ ref_source loc
                  (mention ~case:Subject subject :: [txt " does not allocate"])
              ];
            but
              [code "stack_"; txt " must be applied to something that allocates"]
            |> with_children
                 [ rule
                     [ txt
                         "a record, tuple, array, variant, closure or boxed \
                          field read allocates; a variable, constant or \
                          function result does not" ];
                   rule
                     [ code "stack_";
                       txt
                         " chooses where an allocation happens; it cannot move \
                          a value that already exists" ];
                   suggestion [txt "remove the "; code "stack_"] ] ] ]
    end
  | Constructor_submode_failed { loc; error = e } ->
    let extra_rules _axis =
      { no_extra_rules with
        for_actual =
          [ rule
              [txt "all argument types must mode-cross for rebinding to succeed"]
          ]
      }
    in
    mode_fragments ~error_loc ~extra_rules
      ~actuality_note:Arguments_do_not_cross
      (Mode.With_regionality.fold_error ~init:[] ~step:fold_step
         (loc, Mode.Hint.Unknown) e)
  | Local_value_used_in_exclave { loc; description = desc } ->
    let (item : Mode.Hint.lock_item), name =
      match desc with
      | Mode.Hint.Ident { category; lid } -> category, longident_name lid
      | Mode.Hint.Structure_item (category, id) ->
        category, Some (Ident.name id)
      | Mode.Hint.Module | Mode.Hint.Functor | Mode.Hint.Functor_parameter
      | Mode.Hint.Structure ->
        Module, None
      | Mode.Hint.Class | Mode.Hint.Object -> Class, None
      | Unknown | Function | Parameter | Return | Lazy | Quote | Allocation
      | Expression | Effect_match | Effect_try | Loop | Letop | Cases_result
      | Pattern ->
        Value, None
    in
    let kind =
      match (item : Mode.Hint.lock_item) with
      | Value -> "value"
      | Module -> "module"
      | Constructor -> "constructor"
      | Class -> "class"
    in
    let subject =
      match name with
      | Some name -> Noun.named ~loc ~kind name
      | None -> Noun.described ~loc ("this " ^ kind)
    in
    [ block
        [ Dsl.(state (clause subject (is local_property)))
          |> with_children
               (match item with
               | Class ->
                 [ Dsl.(
                     rule
                       (clause
                          (noun ~number:Plural "classes")
                          (is (always local_property)))) ]
               | Value | Module | Constructor -> []);
          Dsl.(but (clause subject (used_inside (named "exclave_"))))
          |> with_children
               [ rule
                   [ code "exclave_";
                     txt " ends the current region early, so the region's ";
                     local;
                     txt " values cannot be used inside it" ] ] ] ]
  | Mutable_value_used_in_closure
      { loc; pinpoint = boundary_loc, boundary_desc } ->
    let subject = Noun.described ~loc "this variable" in
    [ block
        [ state
            [mention ~case:Subject subject; copula; txt " a mutable variable"];
          but
            [ mention ~case:Subject subject;
              copula;
              txt " used inside ";
              ref_source boundary_loc [txt (human_desc boundary_desc)] ]
          |> with_children
               [ rule
                   [ txt
                       "mutable variables cannot be captured: the capturing \
                        context may outlive them or run in parallel" ];
                 suggestion
                   [ txt "use a ";
                     code "ref";
                     txt " for mutable state shared across functions" ] ] ] ]
  | Unique_use_during_borrowing
      { region_loc; borrow_occ; cannot_force = { occ; axis } } -> begin
    let wanted = match axis with Uniqueness -> unique | Linearity -> once in
    let borrow_loc = borrow_occ.Uniqueness_analysis.Occurrence.loc in
    let subject = Noun.described ~loc:occ.loc "this value" in
    [ block
        [ state [located_mention subject; copula; txt " used as "; wanted];
          but
            [ mention ~case:Subject subject;
              copula;
              txt " ";
              ref_source borrow_loc [txt "borrowed"];
              txt " for the whole of ";
              ref_source region_loc [txt "this borrow"] ]
          |> with_children
               [ rule
                   [ txt
                       "a borrow lends the value for the length of its \
                        context: until the context ends, the value is not the \
                        borrower's to use" ] ] ] ]
    end
  | Uniqueness_error err -> begin
    let used_as (axis : Uniqueness_analysis.Maybe_unique.axis) =
      match axis with Uniqueness -> unique, aliased | Linearity -> many, once
    in
    match err with
    | Uniqueness_analysis.Boundary { cannot_force = { occ; axis }; reason } ->
      let wanted, forced = used_as axis in
      let subject = Noun.described ~loc:occ.loc "this value" in
      let boundary =
        match reason with
        | Uniqueness_analysis.Paths_from_mod_class -> "another module or class"
        | Uniqueness_analysis.Free_var_of_mod_class
        | Uniqueness_analysis.Out_of_mod_class ->
          "outside the current module or class"
      in
      [ block
          [ state [located_mention subject; copula; txt " used as "; wanted];
            but [mention ~case:Subject subject; txt (" comes from " ^ boundary)]
            |> with_children
                 [ rule
                     [ txt "a value that crosses a module or class boundary is ";
                       forced;
                       txt
                         ": the analysis cannot see how the other side uses \
                          it, so it must assume the worst" ] ] ] ]
    | Uniqueness_analysis.Borrowed_value_used_uniquely { occ; axis } ->
      let wanted, forced = used_as axis in
      let subject = Noun.described ~loc:occ.loc "this value" in
      [ block
          [ state [located_mention subject; copula; txt " used as "; wanted];
            but
              [ mention ~case:Subject subject;
                copula;
                txt " borrowed here, which makes ";
                mention ~case:Subject subject;
                txt " ";
                forced ] ] ]
    | Uniqueness_analysis.Borrowed_out_of_context loc ->
      [ block
          [ state
              [ ref_source loc [code "borrow_"];
                txt " is not in a borrowing context" ]
            |> with_children
                 [ rule
                     [ txt
                         "a borrow may be an argument of a function application"
                     ];
                   rule
                     [ txt
                         "a borrow may appear on the right-hand side of a let \
                          binding" ];
                   rule [txt "a borrow may be the scrutinee of a pattern match"]
                 ] ] ]
    | Uniqueness_analysis.Overwrite_changed_tag
        (Uniqueness_analysis.Overwrites.Changed_tag { old_tag; new_tag }) ->
      let tag_name (tag : Uniqueness_analysis.Tag.t) =
        Format_doc.asprintf "%a" Pprintast.Doc.longident tag.name_for_error.txt
      in
      let subject =
        Noun.described ~loc:new_tag.name_for_error.loc "this overwrite"
      in
      let tag = Nlg.subject [Phrase.Text "the overwritten tag"] in
      let contrast : term Phrase.t =
        match old_tag with
        | Uniqueness_analysis.Overwrites.Old_tag_unknown ->
          [mention ~case:Subject tag; copula; txt " not known here"]
        | Uniqueness_analysis.Overwrites.Old_tag_was tag ->
          [ mention ~case:Subject subject;
            txt " overwrites ";
            ref_source tag.name_for_error.loc [code (tag_name tag)] ]
        | Uniqueness_analysis.Overwrites.Old_tag_mutated order ->
          let mutation =
            match order with
            | Uniqueness_analysis.Par -> [copula; txt " being changed"]
            | Uniqueness_analysis.Seq_before | Uniqueness_analysis.Seq_after ->
              [txt " was changed"]
          in
          mention ~case:Subject tag
          :: (mutation
             @ [ txt " by a mutation, so ";
                 mention ~case:Subject tag;
                 copula;
                 txt " not known here" ])
      in
      [ block
          [ state
              [ ref_source new_tag.name_for_error.loc
                  [ mention ~case:Subject subject;
                    txt " sets the tag to ";
                    code (tag_name new_tag) ] ];
            but contrast
            |> with_children
                 [ rule
                     [ txt
                         "an overwrite reuses the block it is given, and the \
                          garbage collector does not support changing a \
                          block's tag: the constructor must stay the same" ] ]
          ] ]
    | Uniqueness_analysis.Cannot_force
        { inner = { cannot_force = { occ; axis }; there; order };
          first_is_of_second
        } -> (
      match Uniqueness_analysis.Usage.extract_occurrence there with
      | None -> []
      | Some there_occ ->
        let here = occ, "used" in
        let other = there_occ, describe_usage there in
        let (first, first_usage), (second, second_usage), second_is_here =
          match order with
          | Uniqueness_analysis.Seq_before -> here, other, false
          | Uniqueness_analysis.Seq_after -> other, here, true
          | Uniqueness_analysis.Par ->
            if
              Location.compare occ.Uniqueness_analysis.Occurrence.loc
                there_occ.Uniqueness_analysis.Occurrence.loc
              < 0
            then here, other, false
            else other, here, true
        in
        let first_loc = first.Uniqueness_analysis.Occurrence.loc in
        let second_loc = second.Uniqueness_analysis.Occurrence.loc in
        let value = Noun.described ~loc:second_loc "this value" in
        let already =
          match order with
          | Uniqueness_analysis.Seq_before | Uniqueness_analysis.Seq_after ->
            [txt " has already been "]
          | Uniqueness_analysis.Par -> [copula; txt " also being "]
        in
        let subject : term Phrase.t =
          match first_is_of_second with
          | Uniqueness_analysis.Self
          | Uniqueness_analysis.Ancestor [Memory_address]
          | Uniqueness_analysis.Descendant [Memory_address] ->
            [mention ~case:Subject value]
          | Uniqueness_analysis.Descendant _ ->
            [txt "part of "; mention ~case:Subject value]
          | Uniqueness_analysis.Ancestor _ ->
            [mention ~case:Subject value; copula; txt " part of a value that"]
        in
        let mode_word, (rule_words : term Phrase.t) =
          match axis with
          | Uniqueness ->
            ( unique,
              [ txt "a value used as ";
                unique;
                txt " must have no other use: that is what ";
                unique;
                txt " means" ] )
          | Linearity ->
            once, [txt "a "; once; txt " value may be used at most once"]
        in
        let first_ref = ref_source first_loc [txt first_usage] in
        let subject_already = Nonempty.append subject already in
        let (statement : term Phrase.t), (contrast : term Phrase.t) =
          if second_is_here
          then
            ( (match axis with
              | Uniqueness ->
                [ ref_source second_loc
                    [ mention ~case:Subject value;
                      copula;
                      txt (" " ^ second_usage ^ " here as ");
                      mode_word ] ]
              | Linearity ->
                [ mention ~case:Subject value;
                  copula;
                  txt " ";
                  mode_word;
                  ref_source second_loc [txt (" and " ^ second_usage ^ " here")]
                ]),
              Nonempty.append subject_already [first_ref] )
          else
            ( [ ref_source second_loc
                  [ mention ~case:Subject value;
                    copula;
                    txt (" " ^ second_usage ^ " here") ] ],
              match axis with
              | Uniqueness ->
                Nonempty.append subject_already
                  [ref_source first_loc [txt (first_usage ^ " as "); mode_word]]
              | Linearity ->
                Nonempty.append subject
                  ([copula; txt " "; mode_word; txt " and"]
                  @ already @ [first_ref]) )
        in
        [ block
            [state statement; but contrast |> with_children [rule rule_words]]
        ])
    end
  | Folded_mismatch axes -> mode_fragments ~error_loc axes

let diagnose ~loc error =
  Diagnostic_term.diagnose ~loc (fun () -> diagnose ~error_loc:loc error)
