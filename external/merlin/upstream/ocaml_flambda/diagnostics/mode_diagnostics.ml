module Nlg = Diagnostic_nlg
module Phrase = Nlg.Phrase
module Step_mode = Mode.Reported_mode
module Side = Diagnostic_term.Side

type mismatch_step =
  { mode : Step_mode.t;
    pinpoint : Mode.Hint.pinpoint;
    kind : Mode.Reported_hint.t
  }

type mode_description = Step_mode.described list

type term = Diagnostic_term.t

type subject = Nlg.subject

type story = term Nlg.story

let plain_story ~claim ?contrast ?background ?suggestions () : story list =
  [Nlg.plain ~claim ?contrast ?background ?suggestions ()]

let described_point (description : Step_mode.described) = description.semantic

let mode_word = Diagnostic_term.mode_word

let modality_word = Diagnostic_term.modality_word

let mode_const_word = Diagnostic_term.mode_const_word

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
    | User_annotation of Location.t
    | User_modality_annotation of string Location.loc
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

  let annotation_meaning mode ({ written_modes; _ } : Mode.Hint.annotation) =
    let mode_name = Step_mode.name mode in
    match
      List.find_opt
        (fun (written_mode : string Location.loc) ->
          String.equal written_mode.txt mode_name)
        written_modes
    with
    | Some written_mode -> User_annotation written_mode.loc
    | None -> Unexplained

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
    | Const (Modality_annotation { annotated_modes; contained_by }) -> (
      match List.assoc_opt (Step_mode.name s.mode) annotated_modes with
      | Some written -> User_modality_annotation written
      | None -> (
        match contained_by with
        | Some containing -> Reroute (Contained_by containing)
        | None -> Unexplained))
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

  let is_region_escape : fact -> bool = function
    | Region_escape _ -> true
    | Mutable_read _ | Mutable_write _ | Lazy_allocated_on_heap | Lazy_forced
    | Module_allocated_on_heap | Legacy_construct _ | Toplevel_expression
    | Tailcall_function | Tailcall_argument | Function_return_default
    | Stack_allocated | Always_dynamic _ | Has_branches
    | Layout_poly_instantiated | Borrowed | Quoted_computation | Spliced
    | Static_not_guaranteed _ ->
      false
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
        | ( Unexplained | User_annotation _
        | User_modality_annotation _ | Capture _ | Signature_argument _
          | Fact _ | Reroute _ ) as says ->
          Some { pinpoint = s.pinpoint; mode = s.mode; says })
      chain

  let origin (chain : t list) =
    match List.rev chain with [] -> None | s :: _ -> Some s

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
      if transparent then for_explanation rest
      else s :: for_explanation rest

  let is_capture (s : t) =
    match s.says with
    | Capture _ -> true
    | Nothing_to_say | Unexplained | User_annotation _
    | User_modality_annotation _ | Signature_argument _
    | Fact _ | Reroute _ ->
      false

  let is_region_escape (s : t) =
    match s.says with
    | Fact fact -> Meaning.is_region_escape fact
    | Nothing_to_say | Unexplained | User_annotation _
    | User_modality_annotation _ | Capture _
    | Signature_argument _ | Reroute _ ->
      false

  let any_escapes_region chain = List.exists is_region_escape chain

  let mutable_read (s : t) : Mode.Hint.mutable_part option =
    match s.says with
    | Fact (Mutable_read part) -> Some part
    | Fact _ | Nothing_to_say | Unexplained | User_annotation _
    | User_modality_annotation _ | Capture _
    | Signature_argument _ | Reroute _ ->
      None

  let mutable_write (s : t) : Mode.Hint.mutable_part option =
    match s.says with
    | Fact (Mutable_write part) -> Some part
    | Fact _ | Nothing_to_say | Unexplained | User_annotation _
    | User_modality_annotation _ | Capture _
    | Signature_argument _ | Reroute _ ->
      None

  let is_function_return (s : t) =
    match s.says with
    | Fact Function_return_default -> true
    | Fact _ | Nothing_to_say | Unexplained | User_annotation _
    | User_modality_annotation _ | Capture _
    | Signature_argument _ | Reroute _ ->
      false

  let no_reroutes (chain : t list) =
    List.for_all
      (fun (s : t) ->
        match s.says with
        | Nothing_to_say | Unexplained | User_annotation _
        | User_modality_annotation _ | Capture _ | Fact _
        | Signature_argument _ ->
          true
        | Reroute _ -> false)
      chain
end

let word_segment = function
  | Phrase.Text text -> Nlg.txt text
  | Phrase.Code text -> Nlg.code text

let subject_words subject = List.map word_segment subject.Nlg.name

let human_desc : Mode.Hint.pinpoint_desc -> string = function
  | Unknown -> "this value"
  | Ident _ -> "this identifier"
  | Function -> "the function"
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

let subject_of_loc ~fallback loc =
  let span = if Location.is_none loc then None else Some loc in
  Nlg.subject ?span [Phrase.Text fallback]

let subject_of_pinpoint ((loc, desc) : Mode.Hint.pinpoint) =
  match desc with
  | Ident { category; lid } ->
    let noun =
      match (category : Mode.Hint.lock_item) with
      | Value -> "the value "
      | Module -> "the module "
      | Class -> "the class "
      | Constructor -> "the constructor "
    in
    let name = Format_doc.asprintf "%a" Printtyp.Doc.longident lid in
    let span = if Location.is_none loc then None else Some loc in
    Nlg.subject ?span [Phrase.Text noun; Phrase.Code name]
  | Structure_item (_, id) ->
    Nlg.subject ~span:loc [Phrase.Code (Ident.name id)]
  | Unknown | Function | Module | Functor | Functor_parameter | Parameter
  | Return | Structure | Lazy | Quote | Allocation | Expression
  | Effect_match | Effect_try | Class | Object | Loop | Letop | Cases_result
  | Pattern -> subject_of_loc ~fallback:(human_desc desc) loc

let short_subject (subject : subject) =
  match subject.name with
  | [Phrase.Text _; Phrase.Code name] ->
    { subject with name = [Phrase.Code name] }
  | _ -> subject

let subject_of_chain (pinpoint : Mode.Hint.pinpoint) (chain : Step.t list) =
  let subject = subject_of_pinpoint pinpoint in
  match snd pinpoint, chain with
  | (Ident { category = Value; _ } | Structure_item (Value, _)),
    { says =
        Capture
          { relation = Closes_over;
            details = { closure = _, Function; _ };
            _
          };
      _
    }
    :: _ ->
    { subject with
      name = Phrase.Text "the function " :: (short_subject subject).name
    }
  | _ -> subject

let description_words (description : mode_description) :
    term Phrase.segment list =
  let open Nlg in
  match description with
  | [] -> []
  | first :: alternatives ->
    mode_word (described_point first)
    :: List.concat_map
         (fun alternative ->
           [txt " or "; mode_word (described_point alternative)])
         alternatives

let mutable_part_noun (part : Mode.Hint.mutable_part) :
    term Phrase.segment list * Phrase.number =
  let open Nlg in
  match part with
  | Record_field f -> [txt "mutable field "; code f], Singular
  | Array_elements -> [txt "array elements"], Plural

let containing_text (containing : Mode.Hint.containing) =
  let with_modality noun = noun ^ ", with some modality" in
  match containing with
  | Tuple -> "as an element of the tuple"
  | Record (field, Modality) ->
    with_modality ("as field " ^ field ^ " of the record")
  | Array Modality -> with_modality "as an element of the array"
  | Constructor (name, Modality) -> with_modality ("via constructor " ^ name)
  | Structure (_, Modality) -> with_modality "in the structure"

let modality_annotation_reason ~mode_name ~subject:owner ?(asides = [])
    (written : string Location.loc) =
  let open Nlg in
  let implication =
    if String.equal mode_name written.txt then []
    else
      [ background
          [ code mode_name;
            txt " is implied by the ";
            code written.txt;
            txt " modality" ] ]
  in
  note ~subject:owner ~asides:(asides @ implication)
    [ txt "because ";
      mention ~case:Subject owner;
      copula;
      txt " annotated ";
      ref_source written.loc
        [term (Diagnostic_term.Written_modality_term written.txt)] ]

let say_step ~side ~asides ~subject:(owner : subject) (s : Step.t) :
    term Nlg.aside list =
  let subj = Nlg.mention ~case:Subject owner in
  let subject_possessive = Nlg.mention ~case:Possessive owner in
  let subject_pronoun = Nlg.pronoun ~case:Possessive owner in
  let say segments = Nlg.note ~asides (Nlg.txt "because " :: segments) in
  let about segments =
    Nlg.note ~subject:owner ~asides (Nlg.txt "because " :: segments)
  in
  let mode =
    Step_mode.describe
      (Side.select side ~expected:`Expected ~actual:`Actual)
      s.mode
    |> description_words
  in
  let open Nlg in
  let is_ rest = [say [subj; copula; txt (" " ^ rest)]] in
  let mutable_access part verb =
    let noun, number = mutable_part_noun part in
    [ say
        ((subject_possessive :: txt " " :: noun)
        @ [copula_agreeing number; txt (" being " ^ verb)]) ]
  in
  match s.says with
  | Nothing_to_say | Unexplained -> []
  | User_modality_annotation annotation ->
    [ modality_annotation_reason ~mode_name:(Step_mode.name s.mode)
        ~subject:owner ~asides annotation ]
  | User_annotation annotation ->
    [ about
        [ subj;
          ref_source annotation
            (copula :: txt " annotated as " :: mode) ] ]
  | Capture { relation = Closes_over; details = { closed; _ }; _ } ->
    [ about
        [ subj;
          txt " closes over ";
          Nlg.mention ~case:Subject (subject_of_pinpoint closed) ] ]
  | Capture { relation = Used_inside; details = { closure; _ }; _ } ->
    [ about
        [ subj;
          copula;
          txt " used inside ";
          Nlg.mention ~case:Subject (subject_of_pinpoint closure) ] ]
  | Signature_argument
      { callee; argument; parameter = { label; index_in_callee_arrow_type } } ->
    let callee = short_subject (subject_of_pinpoint callee) in
    let argument = short_subject (subject_of_pinpoint argument) in
    let position =
      match (label : Mode.Hint.argument_label) with
      | Labelled label | Position label -> [code ("~" ^ label); txt " argument"]
      | Optional label -> [code ("?" ^ label); txt " argument"]
      | Unlabelled ->
        [txt (Nlg.ordinal (index_in_callee_arrow_type + 1) ^ " argument")]
    in
    [ say
        ([Nlg.mention ~case:Subject callee; txt " requires its "]
        @ position
        @ [txt ", "; Nlg.mention ~case:Subject argument; txt ","]
        @ (txt " to be " :: mode)) ]
  | Fact (Mutable_read part) -> mutable_access part "read"
  | Fact (Mutable_write part) -> mutable_access part "written"
  | Fact Lazy_allocated_on_heap ->
    [about [subj; copula; txt " a lazy expression allocated on the heap"]]
  | Fact Module_allocated_on_heap ->
    [about [subj; copula; txt " a module allocated on the heap"]]
  | Fact (Legacy_construct legacy) ->
    let what =
      match (legacy : Mode.Hint.legacy) with
      | Toplevel -> "a top-level definition"
      | Compilation_unit -> "a compilation unit"
      | Class -> "a class"
      | Quoted -> "a quoted expression's result"
    in
    [ about
        [subj; copula; txt (" " ^ what ^ ", which always has the legacy modes")]
    ]
  | Fact Layout_poly_instantiated ->
    [about [subj; copula; txt " layout-polymorphic and instantiated here"]]
  | Fact Lazy_forced -> is_ "a lazy value being forced"
  | Fact Toplevel_expression -> is_ "a top-level expression"
  | Fact Tailcall_function -> is_ "the function of a tail call"
  | Fact Tailcall_argument -> is_ "an argument of a tail call"
  | Fact Function_return_default -> is_ "returned from a function"
  | Fact Stack_allocated ->
    [say [subj; copula; txt " allocated with "; code "stack_"]]
  | Fact (Always_dynamic x) ->
    let what =
      match (x : Mode.Hint.always_dynamic) with
      | Application -> "function applications"
      | Try_with -> "try-with clauses"
      | Generative_functor -> "generative functor applications"
    in
    [say [txt (what ^ " are always dynamic")]]
  | Fact Has_branches -> [say [subj; txt " has branches"]]
  | Fact Borrowed -> is_ "borrowed"
  | Fact (Region_escape (loc, Borrow)) ->
    let escape = txt " escapes a borrow region" in
    [ say
        [ subj;
          (if Location.is_none loc then escape else ref_source loc [escape]) ]
    ]
  | Fact Quoted_computation -> is_ "the quote of a computation"
  | Fact Spliced -> is_ "spliced"
  | Fact (Static_not_guaranteed (Some unit)) ->
    [ say
        [ code (Compilation_unit.name_as_string unit);
          txt
            " is neither a core library nor the current library, and only \
             those can be ";
          mode_const_word (Monadic Staticity) Mode.Staticity.Static ] ]
  | Fact (Static_not_guaranteed None) ->
    [ say
        [ txt "parameter modules are always ";
          mode_const_word (Monadic Staticity) Mode.Staticity.Dynamic ] ]
  | Reroute Mode_crossing ->
    [say [subj; txt " crosses modes based on "; subject_pronoun; txt " type"]]
  | Reroute Partial_application_capture ->
    is_ "captured by a partial application"
  | Reroute (Allocation { txt = desc; loc }) ->
    let located words =
      if Location.is_none loc then words else [ref_source loc words]
    in
    let specific =
      match (desc : Mode.Hint.allocation_desc) with
      | Unknown -> [copula; txt " an allocation"]
      | Optional_argument -> [copula; txt " boxed as an optional argument"]
      | Function_coercion -> [copula; txt " partially applied"]
      | Float_projection -> [copula; txt " a float-record projection"]
      | Lpoly_captured_environment ->
        [txt " captures a layout-polymorphic environment"]
      | Captured_by_partial_application ->
        [copula; txt " captured by a partial application"]
    in
    [about (subj :: located specific)]
  | Reroute (Contains { containing; contained }) ->
    let contained = subject_of_pinpoint contained in
    [ say
        [ subj;
          txt " contains ";
          Nlg.mention ~case:Subject contained;
          txt (" (" ^ containing_text containing ^ ")") ] ]
  | Reroute (Contained_by { containing; container }) ->
    let container = subject_of_pinpoint container in
    [ say
        [ subj;
          copula;
          txt " contained in ";
          Nlg.mention ~case:Subject container;
          txt (" (" ^ containing_text containing ^ ")") ] ]
  | Reroute (Shared_staticity shared) ->
    let related =
      match shared with
      | Of_functor loc -> subject_of_loc ~fallback:"the functor" loc
      | Of_functor_parameter loc ->
        subject_of_loc ~fallback:"the functor parameter" loc
    in
    [ say
        [ subj;
          txt " shares the staticity of ";
          Nlg.mention ~case:Subject related ] ]
  | Reroute (Functor_application loc) ->
    let applied = subject_of_loc ~fallback:"the functor" loc in
    [ say
        [ subj;
          copula;
          txt " an application of ";
          Nlg.mention ~case:Subject applied ] ]
  | Reroute (Functor_applied_at loc) ->
    let application = subject_of_loc ~fallback:"this application" loc in
    [ say
        [ subj;
          copula;
          txt " applied at ";
          Nlg.mention ~case:Subject application ] ]

let rec explain_chain ~side ~subject (chain : Step.t list) :
    term Nlg.aside list =
  match chain with
  | [] -> []
  | { says = Nothing_to_say | Unexplained; _ } :: rest ->
    explain_chain ~side ~subject rest
  | ({ says = Capture { relation; details; source_side }; _ } as step)
    :: rest ->
    let pinpoint =
      match relation with
      | Closes_over -> details.closed
      | Used_inside -> details.closure
    in
    let source = subject_of_chain pinpoint rest in
    let asides =
      match rest with
      | [] -> []
      | next :: _ ->
        let predicate =
          match relation, source_side with
          | _, Actual -> " "
          | Closes_over, Expected -> " used as "
          | Used_inside, Expected -> " expected to be "
        in
        let mode =
          Step_mode.describe
            (Side.select source_side ~expected:`Expected ~actual:`Actual)
            next.mode
          |> description_words
        in
        (* Introduce the next subject by name; its reasons may use pronouns. *)
        [ Nlg.note
            ~asides:(explain_chain ~side:source_side ~subject:source rest)
            (Nlg.txt "and " :: Nlg.mention ~case:Subject source :: Nlg.copula
            :: Nlg.txt predicate :: mode) ]
    in
    say_step ~side ~asides ~subject step
  | step :: rest ->
    let next_subject =
      match rest with
      | [] -> subject
      | next :: _ ->
        let same_location =
          Structured_diagnostic.Location_key.equal
            (Structured_diagnostic.Location_key.of_location (fst step.pinpoint))
            (Structured_diagnostic.Location_key.of_location (fst next.pinpoint))
        in
        if same_location then subject else subject_of_pinpoint next.pinpoint
    in
    let asides = explain_chain ~side ~subject:next_subject rest in
    say_step ~side ~asides ~subject step

module Rule = struct
  type accessed =
    | Field
    | Array_elements

  type mutable_axis =
    | On_contention
    | On_visibility

  type t =
    | Nonportable_closure
    | Portable_function_contends_captures
    | Mutable_write_requirement of accessed * mutable_axis
    | Mutable_read_requirement of accessed * mutable_axis
    | Local_escape

  let explains : t -> Side.t = function
    | Nonportable_closure | Portable_function_contends_captures -> Actual
    | Mutable_write_requirement _ | Mutable_read_requirement _ | Local_escape ->
      Expected

  let accessed (part : Mode.Hint.mutable_part) : accessed =
    match part with Record_field _ -> Field | Array_elements -> Array_elements

  let accessed_nouns : accessed -> string * string = function
    | Field -> "a mutable field", "the value"
    | Array_elements -> "mutable array elements", "the array"

  let sentence : t -> term Phrase.segment list =
    let open Nlg in
    function
    | Nonportable_closure ->
      [ txt "a function that closes over ";
        mode_const_word (Monadic Contention) Mode.Contention.Const.Uncontended;
        txt " data";
        copula;
        txt " ";
        mode_const_word (Comonadic Portability)
          Mode.Portability.Const.Nonportable ]
    | Portable_function_contends_captures ->
      [ txt "values used inside a ";
        mode_const_word (Comonadic Portability) Mode.Portability.Const.Portable;
        txt " function";
        copula_agreeing Plural;
        txt " ";
        mode_const_word (Monadic Contention) Mode.Contention.Const.Contended ]
    | Mutable_write_requirement (accessed, mutable_axis) ->
      let part, owner = accessed_nouns accessed in
      txt ("writing " ^ part ^ " requires " ^ owner ^ " to be ")
      ::
      (match mutable_axis with
      | On_contention ->
        [mode_const_word (Monadic Contention) Mode.Contention.Const.Uncontended]
      | On_visibility ->
        [ mode_const_word (Monadic Visibility) Mode.Visibility.Const.Write;
          txt " or ";
          mode_const_word (Monadic Visibility) Mode.Visibility.Const.Read_write
        ])
    | Mutable_read_requirement (accessed, mutable_axis) ->
      let part, owner = accessed_nouns accessed in
      txt ("reading " ^ part ^ " requires " ^ owner ^ " to be ")
      ::
      (match mutable_axis with
      | On_contention ->
        [ mode_const_word (Monadic Contention) Mode.Contention.Const.Shared;
          txt " or ";
          mode_const_word (Monadic Contention) Mode.Contention.Const.Uncontended
        ]
      | On_visibility ->
        [ mode_const_word (Monadic Visibility) Mode.Visibility.Const.Read;
          txt " or ";
          mode_const_word (Monadic Visibility) Mode.Visibility.Const.Read_write
        ])
    | Local_escape ->
      [ mode_const_word (Comonadic Areality) Mode.Locality.Const.Local;
        txt " values cannot escape their region" ]

  let same_alloc_axis (Mode.Alloc.Axis.P left) (Mode.Alloc.Axis.P right) =
    Int.equal (Mode.Alloc.Axis.compare left right) 0

  let detect ~axis ~actual ~expected : t list =
    let on axis' = same_alloc_axis axis (Mode.Alloc.Axis.P axis') in
    let actual_is_closure =
      Step.no_reroutes actual && List.exists Step.is_capture actual
    in
    let mutable_axis =
      if on (Monadic Contention)
      then Some On_contention
      else if on (Monadic Visibility)
      then Some On_visibility
      else None
    in
    let mutable_requirement part_of =
      Option.bind mutable_axis (fun mutable_axis ->
          match
            ( Option.bind (Step.origin actual) part_of,
              Option.bind (Step.origin expected) part_of )
          with
          | Some part, _ | None, Some part -> Some (accessed part, mutable_axis)
          | None, None -> None)
    in
    List.filter_map Fun.id
      [ (if on (Comonadic Portability) && actual_is_closure
         then Some Nonportable_closure
         else None);
        (if on (Monadic Contention) && actual_is_closure
         then Some Portable_function_contends_captures
         else None);
        Option.map
          (fun (accessed, mutable_axis) ->
            Mutable_write_requirement (accessed, mutable_axis))
          (mutable_requirement Step.mutable_write);
        Option.map
          (fun (accessed, mutable_axis) ->
            Mutable_read_requirement (accessed, mutable_axis))
          (mutable_requirement Step.mutable_read);
        (if Step.any_escapes_region actual || Step.any_escapes_region expected
         then Some Local_escape
         else None) ]
end

let plan_rules ~axis ~actual ~expected ~explains : term Nlg.aside list =
  Rule.detect ~axis ~actual ~expected
  |> List.filter (fun rule -> Side.equal (Rule.explains rule) explains)
  |> List.map (fun rule -> Nlg.background (Rule.sentence rule))

let plan_suggestions ~(expected : Step.t list) : term Nlg.aside list =
  let open Nlg in
  let function_return_origin =
    match Step.origin expected with
    | None -> false
    | Some origin -> Step.is_function_return origin
  in
  if function_return_origin
  then
    [ Nlg.suggest
        [ txt "use ";
          code "exclave_";
          txt " to return a ";
          mode_const_word (Comonadic Areality) Mode.Locality.Const.Local;
          txt " value" ] ]
  else []

let plan_partial_application_hint ~(axis : Mode.Alloc.Axis.packed)
    (result_type : Types.type_expr) : term Nlg.aside list =
  match axis with
  | Mode.Alloc.Axis.P (Mode.Alloc.Axis.Comonadic Areality) -> begin
    let rec non_local_arity sure n ty =
      match Types.get_desc ty with
      | Types.Tarrow ((_, _, res_mode), _, res_ty, _) ->
        begin match
          Mode.Locality.Guts.check_const
            (Mode.Alloc.proj_comonadic Areality res_mode)
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
      [ Nlg.background [Nlg.txt "this is a partial application"];
        Nlg.suggest
          [ Nlg.txt
              ("adding " ^ string_of_int n ^ " more " ^ arguments ^ " "
             ^ qualifier ^ " make the value non-local") ] ]
    end
  | Mode.Alloc.Axis.P _ -> []

type actuality_note = Arguments_do_not_cross

type extra_rules =
  { for_actual : term Nlg.aside list;
    for_expected : term Nlg.aside list
  }

let no_extra_rules = { for_actual = []; for_expected = [] }

type axis_input =
  { axis : Mode.Alloc.Axis.packed;
    actual : mismatch_step list;
    expected : mismatch_step list;
    actual_description : mode_description;
    expected_description : mode_description;
    actual_loosened : bool;
    expected_loosened : bool
  }

let loosened_comparative loosened ~(side : Side.t) =
  if loosened
  then Side.select side ~expected:"stronger than " ~actual:"weaker than "
  else ""

let signature_reason ~axis ~subject:owner
    (declaration : Types.value_description option) : term Nlg.aside list =
  let open Nlg in
  match declaration with
  | None -> []
  | Some declaration ->
    let modalities = declaration.val_modalities in
    let constant =
      if Mode.Modality.is_undefined modalities then None
      else Mode.Modality.to_const_opt modalities
    in
    begin match constant with
    | None -> []
    | Some modalities ->
      let (Mode.Modality.Axis.P axis) =
        Mode.Modality.Axis.of_value (Mode.Const.Axis.alloc_as_value axis)
      in
      let modality = Mode.Modality.Const.proj axis modalities in
      if Mode.Modality.Per_axis.is_id axis modality then []
      else
        match Mode.Modality.Const.annotation axis modalities with
        | Some written ->
          let mode_name =
            Format_doc.asprintf "%a" (Mode.Modality.Per_axis.print axis)
              modality
          in
          [modality_annotation_reason ~mode_name ~subject:owner written]
        | None ->
        [ Nlg.note
            [ txt "because ";
              Nlg.pronoun ~case:Possessive owner;
              txt " signature requires ";
              ref_source declaration.val_loc
                [modality_word (Mode.Modality.Atom (axis, modality))] ] ]
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
      axis_input) : term Nlg.story list =
  let open Nlg in
  let actual = Step.of_chain actual in
  let expected = Step.of_chain expected in
  let subject =
    match (subject_override : subject option) with
    | Some subject -> subject
    | None -> (
      match actual with
      | (s : Step.t) :: _ -> subject_of_pinpoint s.pinpoint
      | [] -> subject_of_loc ~fallback:"this value" error_loc)
  in
  let step_asides side chain =
    explain_chain ~side ~subject (Step.for_explanation chain)
  in
  let actuality_explanation =
    match actuality_note with
    | None -> []
    | Some Arguments_do_not_cross ->
      let axis_name =
        match axis with
        | Mode.Alloc.Axis.P axis ->
          Format_doc.asprintf "%a" Mode.Alloc.Axis.print axis
      in
      [ Nlg.note
          [ txt "the argument types of ";
            Nlg.mention ~case:Subject subject;
            txt (" do not all cross " ^ axis_name) ] ]
  in
  let signature_reason =
    if List.exists
         (fun (step : Step.t) ->
           match step.says with User_modality_annotation _ -> true | _ -> false)
         expected
    then []
    else signature_reason ~axis ~subject expected_declaration
  in
  let expected_beat =
    Nlg.claim ~subject
      ~asides:
        (step_asides Expected expected
        @ signature_reason
        @ plan_rules ~axis ~actual ~expected ~explains:Expected
        @ extra_rules.for_expected @ plan_suggestions ~expected)
      (Nlg.mention ~case:Subject subject
      :: copula
      :: txt
           (" expected to be "
           ^ loosened_comparative expected_loosened ~side:Expected)
      :: description_words expected_description)
  in
  let actual_beat =
    Nlg.but ~subject
      ~asides:
        (step_asides Actual actual @ actuality_explanation
        @ plan_rules ~axis ~actual ~expected ~explains:Actual
        @ extra_rules.for_actual)
      (Nlg.mention ~case:Subject subject
      :: copula
      :: txt (" " ^ loosened_comparative actual_loosened ~side:Actual)
      :: description_words actual_description)
  in
  pronominalize [expected_beat; actual_beat]

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
  | Some (Mode.Alloc.Atom (axis, _)) ->
    Some
      { axis = Mode.Alloc.Axis.P axis;
        actual;
        expected;
        actual_description = Step_mode.describe `Actual actual_mode;
        expected_description = Step_mode.describe `Expected expected_mode;
        actual_loosened;
        expected_loosened
      }

type expression_error =
  | Submode_failed of
      { error : Mode.Value.error;
        context : Typecore.submode_reason
      }
  | Curried_application_complete of
      { label : Typedtree.arg_label;
        error : Mode.Alloc.error;
        part : [`Prefix | `Single_arg | `Entire_apply]
      }
  | Function_mode_mismatch of
      { part : Typecore.mode_mismatch_kind;
        direction : Mode.equate_step;
        error : Mode.Alloc.error
      }
  | Uncurried_function_escapes_comonadic of Mode.Alloc.Comonadic.error
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
        error : Mode.Value.error
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
  { axis : Mode.Value.Axis.packed;
    subject : modality_subject;
    expected : modality_side;
    actual : modality_side;
    requirement : modality_requirement
  }

let modality_story ~(sides : Diagnostic_term.sides) (input : modality_input) :
    term Nlg.story =
  let open Nlg in
  let axis_name =
    match input.axis with
    | Mode.Value.Axis.P ax -> Format_doc.asprintf "%a" Mode.Value.Axis.print ax
  in
  let subject : subject =
    let span =
      match input.actual.loc, input.expected.loc with
      | Some l, _ | None, Some l -> Some l
      | None, None -> None
    in
    match input.subject with
    | Modality_item name -> Nlg.subject ?span [Phrase.Code name]
    | Modality_field name ->
      Nlg.subject ?span [Phrase.Text "the field "; Phrase.Code name]
    | Modality_constructor_arg { constructor; index } ->
      Nlg.subject ?span
        [ Phrase.Text ("the " ^ Nlg.ordinal index ^ " argument of ");
          Phrase.Code constructor ]
  in
  let side ~name ({ atom; loc } : modality_side) : term Phrase.segment list =
    let words =
      match atom with
      | Some atom ->
        copula :: txt " " :: modality_word atom :: txt " in " :: name
      | None -> txt (" has no " ^ axis_name ^ " modality") :: txt " in " :: name
    in
    match loc with None -> words | Some l -> [ref_source l words]
  in
  let header =
    [ txt "the declarations of ";
      Nlg.mention ~case:Subject subject;
      txt (" disagree on " ^ axis_name) ]
  in
  let expected_line =
    Nlg.mention ~case:Subject subject
    :: side ~name:sides.Diagnostic_term.expected_name input.expected
  in
  let actual_line =
    Nlg.mention ~case:Subject subject
    :: side ~name:sides.Diagnostic_term.actual_name input.actual
  in
  let educate =
    match input.requirement with
    | At_least_as_strong -> []
    | Exact_match ->
      [ Nlg.background
          [ txt
              "field and constructor-argument modalities must match exactly on \
               both sides" ] ]
  in
  Nlg.pronominalize_one
    (Nlg.claim ~subject
       ~asides:
         [ Nlg.note ~subject expected_line;
           Nlg.sub_claim ~asides:educate actual_line ]
       header)

let mode_stories ~error_loc ?extra_rules ?actuality_note ?subject_override
    ?expected_declaration
    (axes : mismatch_step list Mode.folded_axis list) : story list =
  List.filter_map prepare_axis axes
  |> List.map (fun (input : axis_input) ->
      let extra_rules =
        match extra_rules with
        | None -> no_extra_rules
        | Some rules -> rules input.axis
      in
      Nlg.story
        (plan_axis ~extra_rules ~actuality_note ~subject_override
           ~expected_declaration ~error_loc input))

let mode_error_stories ~error_loc ?expected_declaration pinpoint error =
  mode_stories ~error_loc ?expected_declaration
    (Mode.Value.fold_error ~init:[] ~step:fold_step pinpoint error)

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
    let open Nlg in
    let fold_value error =
      Mode.Value.fold_error ~init:[] ~step:fold_step
        (loc, Mode.Hint.Expression)
        error
    in
    let fold_alloc error =
      Mode.Alloc.fold_error ~init:[] ~step:fold_step
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
                  [ Nlg.background
                      [ Nlg.txt
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
      mode_stories ~error_loc ~extra_rules ?actuality_note (fold_value e)
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
            (Nlg.subject ~span:loc
               [Phrase.Text "the application up to this argument"])
        | `Entire_apply ->
          Some
            (Nlg.subject ~span:loc
               (Phrase.Text "the application up to " :: argument_words))
      in
      let restricted_word (axis : Mode.Alloc.Axis.packed) =
        match axis with
        | Mode.Alloc.Axis.P (Mode.Alloc.Axis.Comonadic Areality) ->
          Some (mode_const_word (Comonadic Areality) Mode.Locality.Const.Local)
        | Mode.Alloc.Axis.P (Mode.Alloc.Axis.Comonadic Linearity) ->
          Some (mode_const_word (Comonadic Linearity) Mode.Linearity.Const.Once)
        | Mode.Alloc.Axis.P _ -> None
      in
      let suggestion_phrases =
        match part with
        | `Prefix ->
          [[Nlg.txt "try wrapping the marked application in parentheses"]]
        | `Single_arg ->
          [ [Nlg.txt "try splitting the application in two"];
            [ Nlg.txt
                "the arguments after this one in the function's type should be \
                 applied separately" ] ]
        | `Entire_apply ->
          [ [Nlg.txt "try splitting the application in two"];
            Nlg.txt "the arguments after "
            :: List.map word_segment argument_words
            @ [Nlg.txt " in the function's type should be applied separately"] ]
      in
      let extra_rules axis =
        match restricted_word axis with
        | None -> no_extra_rules
        | Some word ->
          { no_extra_rules with
            for_expected =
              Nlg.background
                [ Nlg.txt "when passing or calling ";
                  word;
                  Nlg.txt
                    " values, extra arguments are passed in a separate \
                     application" ]
              :: List.map Nlg.suggest suggestion_phrases
          }
      in
      mode_stories ~error_loc ~extra_rules ?subject_override (fold_alloc e)
    | Function_mode_mismatch { part; direction = step; error = e } ->
      let subject_override : subject option =
        match (part : Typecore.mode_mismatch_kind) with
        | Parameter ->
          Some (Nlg.subject ~span:loc [Phrase.Text "this function's parameter"])
        | Return ->
          Some
            (Nlg.subject ~span:loc [Phrase.Text "this function's return value"])
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
      mode_stories ~error_loc ?subject_override axes
    | Uncurried_function_escapes_comonadic e ->
      let subject_override : subject option =
        Some
          (Nlg.subject ~span:loc
             [Phrase.Text "this function when partially applied"])
      in
      let extra_rules _axis =
        { no_extra_rules with
          for_actual =
            [ Nlg.background
                [ Nlg.txt
                    "partially applying a function closes over the arguments \
                     given so far" ] ]
        }
      in
      mode_stories ~error_loc ~extra_rules ?subject_override
        (fold_alloc (Mode.Alloc.Comonadic e))
    | Overwrite_of_invalid_term ->
      plain_story
        ~claim:[ref_source loc [txt "this term cannot be overwritten"]]
        ~contrast:
          [ txt
              "but overwriting works only on tuples, constructors and boxed \
               records" ]
        ~background:
          [ [ code "overwrite_";
              txt
                " reuses an existing block, so the value must be one that \
                 occupies a block of its own" ] ]
        ()
    | Block_index_modality_mismatch { mutable_elements = mut; error = err } ->
      let _step, Mode.Modality.Error (ax, { left; right = _ }) = err in
      let axis_name =
        match Mode.Modality.Axis.to_value (Mode.Modality.Axis.P ax) with
        | Mode.Value.Axis.P vax ->
          Format_doc.asprintf "%a" Mode.Value.Axis.print vax
      in
      let actual_words =
        if Mode.Modality.Per_axis.is_id ax left
        then [txt ("no modality on the " ^ axis_name ^ " axis")]
        else
          [txt "the modality "; modality_word (Mode.Modality.Atom (ax, left))]
      in
      plain_story
        ~claim:
          (ref_source loc [txt "this block index reaches a field with "]
          :: actual_words)
        ~contrast:
          [ txt
              ("but a block index over "
              ^ (match mut with true -> "mutable" | false -> "immutable")
              ^ " elements requires the modalities implied by its declaration, \
                 and no others") ]
        ~background:
          [ [ txt
                "this is a current limitation: the block-index primitives are \
                 typed with one fixed modality and cannot express others \
                 yet" ] ]
        ~suggestions:
          [ [ txt
                "remove the modality from the field, or read the field \
                 directly instead of taking an index" ] ]
        ()
    | Exclave_in_nontail_position ->
      let subject = subject_of_loc ~fallback:"this expression" loc in
      plain_story
        ~claim:
          [ ref_source loc
              (subject_words subject @ [txt " is not in tail position"]) ]
        ~contrast:
          [ txt "but ";
            code "exclave_";
            txt " must be the last thing the enclosing region evaluates" ]
        ~background:
          [ [ code "exclave_";
              txt
                " puts a value in the caller's region, so it can only appear \
                 where the current region is about to end" ] ]
        ()
    | Exclave_returns_not_local ->
      plain_story
        ~claim:
          [ ref_source loc [txt "this expression is "];
            mode_const_word (Comonadic Areality) Mode.Locality.Const.Local;
            txt ", because ";
            code "exclave_";
            txt " makes it so" ]
        ~contrast:
          [ txt "but the enclosing function is not declared to return a ";
            mode_const_word (Comonadic Areality) Mode.Locality.Const.Local;
            txt " value" ]
        ~background:
          [ [ txt "a function containing ";
              code "exclave_";
              txt " allocates into its caller's region, so it must itself be ";
              mode_const_word (Comonadic Areality) Mode.Locality.Const.Local;
              txt "-returning" ] ]
        ~suggestions:
          [ [ txt "annotate the function's result as ";
              mode_const_word (Comonadic Areality) Mode.Locality.Const.Local;
              txt ", or drop the ";
              code "exclave_" ] ]
        ()
    | Tail_call_local_returning ->
      let subject = subject_of_loc ~fallback:"this call" loc in
      plain_story
        ~claim:
          [ ref_source loc
              (subject_words subject
              @ [ txt " returns a ";
                  mode_const_word (Comonadic Areality)
                    Mode.Locality.Const.Local;
                  txt " value" ]) ]
        ~contrast:
          [ txt "but it is in the tail position of a function that is not ";
            mode_const_word (Comonadic Areality) Mode.Locality.Const.Local;
            txt "-returning" ]
        ~background:
          [ [ txt "a tail call hands its result straight to the caller, so a ";
              mode_const_word (Comonadic Areality) Mode.Locality.Const.Local;
              txt
                "-returning call can only sit in the tail of a local-returning \
                 function" ] ]
        ~suggestions:
          [ [ txt "bind the result first, as in ";
              code "let r = ... in r";
              txt ", so the call is no longer in tail position" ] ]
        ()
    | Always_heap_allocation kind ->
      let what =
        match (kind : Typecore.always_heap_allocation) with
        | Lazy -> "a lazy expression"
        | Module -> "a module"
        | Object -> "an object"
        | List_comprehension -> "a list comprehension"
        | Array_comprehension -> "an array comprehension"
      in
      plain_story
        ~claim:
          [ ref_source loc
              [txt ("the compiler cannot stack-allocate " ^ what ^ " yet")] ]
        ~background:
          [[txt "this is a current limitation, not a rule of the language"]]
        ~suggestions:
          [ [ txt "drop the ";
              code "stack_";
              txt " and let this allocate on the heap" ] ]
        ()
    | Always_static_allocation kind ->
      let what =
        match (kind : Typecore.always_static_allocation) with
        | Constant -> "a literal"
        | Src_pos -> "a source position literal"
        | Unboxed_unit -> "an unboxed unit literal"
        | Unboxed_bool -> "an unboxed boolean literal"
      in
      plain_story
        ~claim:[ref_source loc [txt (what ^ " is not allocated at runtime")]]
        ~contrast:
          [ txt "but ";
            code "stack_";
            txt " must be applied to something that allocates" ]
        ~background:
          [ [ code "stack_";
              txt
                " chooses where an allocation happens, and this value needs no \
                 allocation to choose from" ] ]
        ~suggestions:[[txt "remove the "; code "stack_"]]
        ()
    | Not_allocation ->
      let subject = subject_of_loc ~fallback:"this expression" loc in
      plain_story
        ~claim:
          [ref_source loc (subject_words subject @ [txt " does not allocate"])]
        ~contrast:
          [ txt "but ";
            code "stack_";
            txt " must be applied to something that allocates" ]
        ~background:
          [ [ txt
                "a record, tuple, array, variant, closure or boxed field read \
                 allocates; a variable, constant or function result does not" ];
            [ code "stack_";
              txt
                " chooses where an allocation happens; it cannot move a value \
                 that already exists" ] ]
        ~suggestions:[[txt "remove the "; code "stack_"]]
        ()
    end
  | Constructor_submode_failed { loc; error = e } ->
    let extra_rules _axis =
      { no_extra_rules with
        for_actual =
          [ Nlg.background
              [ Nlg.txt
                  "all argument types must mode-cross for rebinding to \
                   succeed" ]
          ]
      }
    in
    mode_stories ~error_loc ~extra_rules ~actuality_note:Arguments_do_not_cross
      (Mode.Value.fold_error ~init:[] ~step:fold_step
         (loc, Mode.Hint.Unknown) e)
  | Local_value_used_in_exclave { loc; description = desc } ->
    let open Nlg in
    let local_word =
      mode_const_word (Comonadic Areality) Mode.Locality.Const.Local
    in
    let (item : Mode.Hint.lock_item), name =
      match desc with
      | Mode.Hint.Ident { category; lid } -> category, Nlg.longident_name lid
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
    let named noun fallback =
      match name with
      | Some name -> Nlg.subject ~span:loc [Phrase.Text noun; Phrase.Code name]
      | None -> Nlg.subject ~span:loc [Phrase.Text fallback]
    in
    let plainly_local noun fallback =
      let s = named noun fallback in
      s, [Nlg.mention ~case:Subject s; copula; txt " "; local_word]
    in
    let subject, claim =
      match (item : Mode.Hint.lock_item) with
      | Mode.Hint.Value -> plainly_local "the value " "this value"
      | Module -> plainly_local "the module " "this module"
      | Constructor -> plainly_local "the constructor " "this constructor"
      | Class ->
        let s =
          match name with
          | Some name -> Nlg.subject ~span:loc [Phrase.Code name]
          | None -> Nlg.subject ~span:loc [Phrase.Text "this class"]
        in
        ( s,
          [ Nlg.mention ~case:Subject s;
            copula;
            txt " a class, and classes are always ";
            local_word ] )
    in
    [ Nlg.story
        [ Nlg.claim ~subject claim;
          Nlg.but
            ~asides:
              [ Nlg.background
                  [ code "exclave_";
                    txt " ends the current region early, so the region's ";
                    local_word;
                    txt " values cannot be used inside it" ] ]
            [ Nlg.pronoun ~case:Subject subject;
              copula;
              txt " used inside ";
              code "exclave_" ] ] ]
  | Mutable_value_used_in_closure
      { loc; pinpoint = boundary_loc, boundary_desc } ->
    let open Nlg in
    let subject = subject_of_loc ~fallback:"this variable" loc in
    [ Nlg.story
        [ Nlg.claim ~subject
            [ Nlg.mention ~case:Subject subject;
              copula;
              txt " a mutable variable" ];
          Nlg.but
            ~asides:
              [ Nlg.background
                  [ txt
                      "mutable variables cannot be captured: the capturing \
                       context may outlive them or run in parallel" ];
                Nlg.suggest
                  [ txt "use a ";
                    code "ref";
                    txt " for mutable state shared across functions" ] ]
            [ Nlg.pronoun ~case:Subject subject;
              copula;
              txt " used inside ";
              ref_source boundary_loc [txt (human_desc boundary_desc)] ] ] ]
  | Unique_use_during_borrowing
      { region_loc; borrow_occ; cannot_force = { occ; axis } } -> begin
    let open Nlg in
    let wanted =
      match axis with
      | Uniqueness ->
        mode_const_word (Monadic Uniqueness) Mode.Uniqueness.Const.Unique
      | Linearity ->
        mode_const_word (Comonadic Linearity) Mode.Linearity.Const.Once
    in
    let borrow_loc = borrow_occ.Uniqueness_analysis.Occurrence.loc in
    plain_story
      ~claim:[ref_source occ.loc [txt "this value is used as "]; wanted]
      ~contrast:
        [ txt "but it is ";
          ref_source borrow_loc [txt "borrowed"];
          txt " for the whole of ";
          ref_source region_loc [txt "this borrow"] ]
      ~background:
        [ [ txt
              "a borrow lends the value for the length of its context: \
               until the context ends, the value is not the borrower's to use"
          ] ]
      ()
    end
  | Uniqueness_error err -> begin
    let open Nlg in
    let unique_word =
      mode_const_word (Monadic Uniqueness) Mode.Uniqueness.Const.Unique
    in
    let aliased_word =
      mode_const_word (Monadic Uniqueness) Mode.Uniqueness.Const.Aliased
    in
    let once_word =
      mode_const_word (Comonadic Linearity) Mode.Linearity.Const.Once
    in
    let many_word =
      mode_const_word (Comonadic Linearity) Mode.Linearity.Const.Many
    in
    let used_as (axis : Uniqueness_analysis.Maybe_unique.axis) =
      match axis with
      | Uniqueness -> unique_word, aliased_word
      | Linearity -> many_word, once_word
    in
    match err with
    | Uniqueness_analysis.Boundary { cannot_force = { occ; axis }; reason } ->
      let wanted, forced = used_as axis in
      let boundary =
        match reason with
        | Uniqueness_analysis.Paths_from_mod_class -> "another module or class"
        | Uniqueness_analysis.Free_var_of_mod_class
        | Uniqueness_analysis.Out_of_mod_class ->
          "outside the current module or class"
      in
      plain_story
        ~claim:[ref_source occ.loc [txt "this value is used as "]; wanted]
        ~contrast:[txt ("but it comes from " ^ boundary)]
        ~background:
          [ [ txt "a value that crosses a module or class boundary is ";
              forced;
              txt
                ": the analysis cannot see how the other side uses it, so it \
                 must assume the worst" ] ]
        ()
    | Uniqueness_analysis.Borrowed_value_used_uniquely { occ; axis } ->
      let wanted, forced = used_as axis in
      plain_story
        ~claim:[ref_source occ.loc [txt "this value is used as "]; wanted]
        ~contrast:[txt "but it is borrowed here, which makes it "; forced]
        ()
    | Uniqueness_analysis.Borrowed_out_of_context loc ->
      plain_story
        ~claim:
          [ ref_source loc [code "borrow_"];
            txt " is not in a borrowing context" ]
        ~background:
          [ [txt "a borrow may be an argument of a function application"];
            [txt "a borrow may appear on the right-hand side of a let binding"];
            [txt "a borrow may be the scrutinee of a pattern match"] ]
        ()
    | Uniqueness_analysis.Overwrite_changed_tag
        (Uniqueness_analysis.Overwrites.Changed_tag { old_tag; new_tag }) ->
      let tag_name (tag : Uniqueness_analysis.Tag.t) =
        Format_doc.asprintf "%a" Pprintast.Doc.longident tag.name_for_error.txt
      in
      let contrast =
        match old_tag with
        | Uniqueness_analysis.Overwrites.Old_tag_unknown ->
          [txt "but the tag it overwrites is not known here"]
        | Uniqueness_analysis.Overwrites.Old_tag_was tag ->
          [ txt "but it overwrites ";
            ref_source tag.name_for_error.loc [code (tag_name tag)] ]
        | Uniqueness_analysis.Overwrites.Old_tag_mutated order ->
          [ txt
              (match order with
              | Uniqueness_analysis.Par ->
                "but the tag is being changed by a mutation, so it is not \
                 known here"
              | Uniqueness_analysis.Seq_before
              | Uniqueness_analysis.Seq_after ->
                "but the tag was changed by a mutation, so it is not \
                 known here")
          ]
      in
      plain_story
        ~claim:
          [ ref_source new_tag.name_for_error.loc
              [txt "this overwrite sets the tag to "; code (tag_name new_tag)] ]
        ~contrast
        ~background:
          [ [ txt
                "an overwrite reuses the block it is given, and the garbage \
                 collector does not support changing a block's tag: the \
                 constructor must stay the same" ] ]
        ()
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
        let already =
          match order with
          | Uniqueness_analysis.Seq_before | Uniqueness_analysis.Seq_after ->
            "has already been "
          | Uniqueness_analysis.Par -> "is also being "
        in
        let subject =
          match first_is_of_second with
          | Uniqueness_analysis.Self
          | Uniqueness_analysis.Ancestor [Memory_address]
          | Uniqueness_analysis.Descendant [Memory_address] ->
            "it "
          | Uniqueness_analysis.Descendant _ -> "part of it "
          | Uniqueness_analysis.Ancestor _ -> "it is part of a value that "
        in
        let mode_word, rule =
          match axis with
          | Uniqueness ->
            ( mode_const_word (Monadic Uniqueness) Mode.Uniqueness.Const.Unique,
              [ txt "a value used as ";
                unique_word;
                txt " must have no other use: that is what ";
                unique_word;
                txt " means" ] )
          | Linearity ->
            ( once_word,
              [txt "a "; once_word; txt " value may be used at most once"] )
        in
        let first_loc = first.Uniqueness_analysis.Occurrence.loc in
        let second_loc = second.Uniqueness_analysis.Occurrence.loc in
        let first_ref = ref_source first_loc [txt first_usage] in
        let but_already = txt ("but " ^ subject ^ already) in
        let claim, contrast =
          if second_is_here
          then
            ( (match axis with
              | Uniqueness ->
                [ ref_source second_loc
                    [txt ("this value is " ^ second_usage ^ " here as ")];
                  mode_word ]
              | Linearity ->
                [ txt "this value is ";
                  mode_word;
                  ref_source second_loc [txt (" and " ^ second_usage ^ " here")]
                ]),
              [but_already; first_ref] )
          else
            ( [ ref_source second_loc
                  [txt ("this value is " ^ second_usage ^ " here")] ],
              match axis with
              | Uniqueness ->
                [ but_already;
                  ref_source first_loc [txt (first_usage ^ " as ")];
                  mode_word ]
              | Linearity ->
                [ txt ("but " ^ subject ^ "is ");
                  mode_word;
                  txt (" and " ^ already);
                  first_ref ] )
        in
        plain_story ~claim ~contrast ~background:[rule] ())
    end
  | Folded_mismatch axes -> mode_stories ~error_loc axes

let diagnose ~loc error =
  Diagnostic_term.diagnose ~loc (fun () -> diagnose ~error_loc:loc error)
