module Diagnostic = Structured_diagnostic
module Nlg = Diagnostic_nlg
module Phrase = Nlg.Phrase
module Side = Diagnostic_term.Side

type term = Diagnostic_term.t

type story = term Nlg.story

type sides = Diagnostic_term.sides =
  { expected_name : Diagnostic_term.t Phrase.segment list;
    actual_name : Diagnostic_term.t Phrase.segment list
  }

type error =
  | Not_included of
      { loc : Location.t;
        explanation : Includemod.explanation
      }
  | Strengthening_mismatch of
      { loc : Location.t;
        path : Longident.t;
        explanation : Includemod.explanation
      }
  | Applicative_functor_mismatch of
      { loc : Location.t;
        constrained : Longident.t;
        type_path : Path.t;
        explanation : Includemod.explanation
      }
  | Substitution_mismatch of
      { loc : Location.t;
        path : Longident.t;
        explanation : Includemod.explanation
      }
  | Functor_application_mismatch of
      { env : Env.t;
        app_name : Includemod.application_name;
        mty_f : Types.module_type;
        args :
          (Includemod.Error.functor_arg_descr
          * Types.module_type
          * Typedtree.mode_with_locks)
          list
      }
  | Type_definition_mismatch of
      { loc : Location.t;
        type_expr : Types.type_expr;
        env : Env.t;
        mismatch : Includecore.type_mismatch
      }

let declaration_sides : sides =
  { expected_name = [Nlg.txt "the expected declaration"];
    actual_name = [Nlg.txt "the actual declaration"]
  }

module Orientation = struct
  type t =
    | Got_is_actual
    | Got_is_expected

  let reverse = function
    | Got_is_actual -> Got_is_expected
    | Got_is_expected -> Got_is_actual

  let got_side : t -> Side.t = function
    | Got_is_actual -> Actual
    | Got_is_expected -> Expected

  let expected_side t = Side.other (got_side t)

  let side_of_position t (position : Includecore.position) =
    match position with First -> got_side t | Second -> expected_side t

  let expected_and_actual t ~got ~expected =
    match t with
    | Got_is_actual -> expected, got
    | Got_is_expected -> got, expected
end

type modality_subject = Mode_diagnostics.modality_subject =
  | Modality_item of string
  | Modality_field of string
  | Modality_constructor_arg of
      { constructor : string;
        index : int
      }

type modality_side = Mode_diagnostics.modality_side =
  { atom : Mode.Modality.atom option;
    loc : Location.t option
  }

type modality_requirement = Mode_diagnostics.modality_requirement =
  | Exact_match
  | At_least_as_strong

type modality_input = Mode_diagnostics.modality_input =
  { axis : Mode.Value.Axis.packed;
    subject : modality_subject;
    expected : modality_side;
    actual : modality_side;
    requirement : modality_requirement
  }

let modality_input ?(orientation = Orientation.Got_is_actual) ~subject
    ~expected_loc ~actual_loc ~requirement
    (Mode.Modality.Error (ax, { left; right }) : Mode.Modality.error) :
    modality_input =
  let side m loc : modality_side =
    { atom =
        (if Mode.Modality.Per_axis.is_id ax m
         then None
         else Some (Mode.Modality.Atom (ax, m)));
      loc
    }
  in
  let expected, actual =
    Orientation.expected_and_actual orientation ~got:(side left actual_loc)
      ~expected:(side right expected_loc)
  in
  { axis = Mode.Modality.Axis.to_value (Mode.Modality.Axis.P ax);
    subject;
    expected;
    actual;
    requirement
  }

let equate_modality_input ~orientation ~subject ~expected_loc ~actual_loc
    ((_step, error) : Mode.Modality.equate_error) =
  modality_input ~orientation ~subject ~expected_loc ~actual_loc
    ~requirement:Exact_match error

type crossing_difference =
  | Attribute_on_one_side of { declared_on : Side.t }
  | Bounds_differ of
      { expected_only : string list;
        actual_only : string list;
        differing : (string * string * string) list;
        expected_with : string;
        actual_with : string
      }

type crossing_input =
  { difference : crossing_difference;
    expected_loc : Location.t option;
    actual_loc : Location.t option
  }

let collapse_whitespace (s : string) : string =
  let buf = Buffer.create (String.length s) in
  let pending = ref false in
  String.iter
    (fun c ->
      match c with
      | ' ' | '\n' | '\t' -> if Buffer.length buf > 0 then pending := true
      | c ->
        if !pending
        then begin
          Buffer.add_char buf ' ';
          pending := false
        end;
        Buffer.add_char buf c)
    s;
  Buffer.contents buf

let crossing_on_axis (Mode.Value.Axis.P vax as packed) (t : Mode.Crossing.t) :
    (string * string) option =
  match Mode.Crossing.Axis.of_modality (Mode.Modality.Axis.of_value packed) with
  | Mode.Crossing.Axis.P cax ->
    let value = Mode.Crossing.proj cax t in
    if Mode.Crossing.Per_axis.le cax (Mode.Crossing.Per_axis.max cax) value
    then None
    else
      Some
        ( Format_doc.asprintf "%a" Mode.Value.Axis.print vax,
          Format_doc.asprintf "%a" (Mode.Crossing.Per_axis.print cax) value )

let crossing_bounds_difference (expected : Mode.Crossing.t)
    (actual : Mode.Crossing.t) =
  List.fold_left
    (fun (expected_only, actual_only, differing) axis ->
      match crossing_on_axis axis expected, crossing_on_axis axis actual with
      | None, None -> expected_only, actual_only, differing
      | Some (name, _), None -> name :: expected_only, actual_only, differing
      | None, Some (name, _) -> expected_only, name :: actual_only, differing
      | Some (name, e), Some (_, a) ->
        if String.equal e a
        then expected_only, actual_only, differing
        else expected_only, actual_only, (name, e, a) :: differing)
    ([], [], []) Mode.Value.Axis.all
  |> fun (e, a, d) -> List.rev e, List.rev a, List.rev d

let located loc words =
  match loc with None -> words | Some l -> [Nlg.ref_source l words]

let crossing_story ~sides (input : crossing_input) : story =
  let open Nlg in
  let attribute = "[@@unsafe_allow_any_mode_crossing]" in
  let span =
    match input.actual_loc, input.expected_loc with
    | Some l, _ | None, Some l -> Some l
    | None, None -> None
  in
  let subject = Nlg.subject ?span [Phrase.Text "the declarations"] in
  let header =
    [ Nlg.mention ~case:Subject subject;
      txt " disagree on ";
      Diagnostic_term.concept_word Diagnostic_term.Unsafe_mode_crossing ]
  in
  let asides =
    match input.difference with
    | Attribute_on_one_side { declared_on } ->
      let declaring = Diagnostic_term.side_name sides declared_on in
      let other = Diagnostic_term.side_name sides (Side.other declared_on) in
      let side_loc side =
        Side.select side ~expected:input.expected_loc ~actual:input.actual_loc
      in
      let declaring_loc = side_loc declared_on in
      let other_loc = side_loc (Side.other declared_on) in
      [ note
          (located declaring_loc
             (txt "only " :: declaring
             @ [txt " is marked "; code attribute]));
        sub_claim
          ~asides:
            [ background
                [ code attribute;
                  txt
                    " is part of a type's interface: both declarations must \
                     carry it" ] ]
          (located other_loc (txt "but " :: other @ [txt " is not"])) ]
    | Bounds_differ
        { expected_only; actual_only; differing; expected_with; actual_with } ->
      let name_of_expected = sides.expected_name in
      let name_of_actual = sides.actual_name in
      let axes_line ~name ~loc axes =
        match axes with
        | [] -> None
        | axes ->
          let plural = match axes with [_] -> " axis" | _ -> " axes" in
          let crosses = " crosses the " ^ String.concat ", " axes ^ plural in
          Some (note (located loc (txt "only " :: name @ [txt crosses])))
      in
      let differing_lines =
        List.map
          (fun (axis, e, a) ->
            note
              (txt ("both cross the " ^ axis ^ " axis, but ")
               :: located input.expected_loc name_of_expected
              @ txt " to " :: code e :: txt " and "
                :: located input.actual_loc name_of_actual
              @ [txt " to "; code a]))
          differing
      in
      let with_lines =
        if String.equal expected_with actual_with
        then []
        else
          let line ~name ~loc with_ =
            let words =
              (txt "the crossing in " :: name)
              @
              if String.equal with_ ""
              then
                [ txt " has no ";
                  Diagnostic_term.concept_word Diagnostic_term.With_bounds ]
              else [txt " includes "; code with_]
            in
            note (located loc words)
          in
          [ line ~name:sides.expected_name ~loc:input.expected_loc
              expected_with;
            line ~name:sides.actual_name ~loc:input.actual_loc actual_with ]
      in
      let claim_lines =
        List.filter_map Fun.id
          [ axes_line ~name:sides.expected_name ~loc:input.expected_loc
              expected_only;
            axes_line ~name:sides.actual_name ~loc:input.actual_loc
              actual_only ]
        @ differing_lines @ with_lines
      in
      let educate =
        background
          [ txt "two declarations that both use ";
            code attribute;
            txt " must claim exactly the same mode crossing" ]
      in
      claim_lines @ [educate]
  in
  Nlg.pronominalize_one (Nlg.claim ~subject ~asides header)

let declarations_do_not_match ?(background = []) ~noun ~name ~expected_loc
    ~got_loc children : story =
  let span =
    match expected_loc, got_loc with
    | Some loc, _ | None, Some loc -> Some loc
    | None, None -> None
  in
  let subject = Nlg.subject ?span [Phrase.Text noun; Phrase.Code name] in
  Nlg.claim ~subject
    ~asides:(List.map Nlg.child children @ background)
    [ Nlg.txt "the declarations of ";
      Nlg.mention ~case:Subject subject;
      Nlg.txt " do not match" ]

let parameters_do_not_match ~position children : story =
  Nlg.claim
    ~asides:(List.map Nlg.child children)
    (match position with
    | None -> [Nlg.txt "the functors' parameters do not match"]
    | Some position ->
      [ Nlg.txt
          ("the declarations of the " ^ Nlg.ordinal position
         ^ " parameter do not match") ])

let is_not_included_in ~sides ~(not_included : Side.t) children : story =
  let name side = Diagnostic_term.side_name sides side in
  Nlg.claim
    ~asides:(List.map Nlg.child children)
    (name not_included
    @ (Nlg.txt " is not included in " :: name (Side.other not_included)))

let same_children (left : story) (right : story) =
  Diagnostic.Block.equal
    (Diagnostic_term.rendered_children left)
    (Diagnostic_term.rendered_children right)

let frame ~subject predicate children =
  match children with
  | [] -> []
  | children ->
    [ Nlg.reframe
        (Nlg.claim ~subject (Nlg.mention ~case:Subject subject :: predicate))
        children ]

let field_stories ~sides ~orientation
    (changes : Includecore.record_change list) : story list =
  List.filter_map
    (fun (change : Includecore.record_change) ->
      match change with
      | Diffing_with_keys.Change
          (Type
             { got = ld1;
               expected = ld2;
               reason = Includecore.Modality equate;
               _
             }) ->
        Some
          (Mode_diagnostics.modality_story ~sides
             (equate_modality_input ~orientation
                ~subject:(Modality_field (Ident.name ld1.Types.ld_id))
                ~expected_loc:(Some ld2.Types.ld_loc)
                ~actual_loc:(Some ld1.Types.ld_loc) equate))
      | Diffing_with_keys.Change
          (Type
             { reason = Includecore.(Type _ | Mutability _ | Atomicity _); _ })
      | Diffing_with_keys.Change (Name _)
      | Diffing_with_keys.Swap _ | Diffing_with_keys.Move _
      | Diffing_with_keys.Insert _ | Diffing_with_keys.Delete _ ->
        None)
    changes

let constructor_stories ~sides ~orientation
    (changes : Includecore.variant_change list) : story list =
  List.concat_map
    (fun (change : Includecore.variant_change) ->
      match change with
      | Diffing_with_keys.Change
          (Type { got = cd1, _; expected = cd2, _; reason; _ }) -> begin
        match (reason : Includecore.constructor_mismatch) with
        | Includecore.Modality (i, equate) ->
          [ Mode_diagnostics.modality_story ~sides
              (equate_modality_input ~orientation
                 ~subject:
                   (Modality_constructor_arg
                      { constructor = Ident.name cd1.Types.cd_id;
                        index = i + 1
                      })
                 ~expected_loc:(Some cd2.Types.cd_loc)
                 ~actual_loc:(Some cd1.Types.cd_loc) equate) ]
        | Includecore.Inline_record changes ->
          field_stories ~sides ~orientation changes
        | Includecore.(
            ( Type _ | Arity | Kind _ | Explicit_return_type _
            | Fixed_representation _ | Immediate_representation _
            | Constructor_representation_shape_mismatch )) ->
          []
        end
      | Diffing_with_keys.Change (Name _)
      | Diffing_with_keys.Swap _ | Diffing_with_keys.Move _
      | Diffing_with_keys.Insert _ | Diffing_with_keys.Delete _ ->
        [])
    changes

let type_stories ~sides ~orientation ~expected_loc ~actual_loc
    (mismatch : Includecore.type_mismatch) : story list =
  match mismatch with
  | Includecore.Record_mismatch (Includecore.Label_mismatch changes) ->
    field_stories ~sides ~orientation changes
  | Includecore.Variant_mismatch changes ->
    constructor_stories ~sides ~orientation changes
  | Includecore.Unsafe_mode_crossing mismatch ->
    let difference =
      match (mismatch : Includecore.unsafe_mode_crossing_mismatch) with
      | Includecore.Mode_crossing_only_on ord ->
        Attribute_on_one_side
          { declared_on = Orientation.side_of_position orientation ord }
      | Includecore.Bounds_not_equal (got, expected) ->
        let expected, got =
          Orientation.expected_and_actual orientation ~got ~expected
        in
        let with_bounds (umc : Types.unsafe_mode_crossing) =
          collapse_whitespace
            (Format_doc.asprintf "%a" Jkind.With_bounds.format
               umc.Types.unsafe_with_bounds)
        in
        let expected_only, actual_only, differing =
          crossing_bounds_difference
            expected.Types.unsafe_mod_bounds.Types.crossing
            got.Types.unsafe_mod_bounds.Types.crossing
        in
        Bounds_differ
          { expected_only;
            actual_only;
            differing;
            expected_with = with_bounds expected;
            actual_with = with_bounds got
          }
    in
    [crossing_story ~sides { difference; expected_loc; actual_loc }]
  | Includecore.(
      ( Arity | Privacy _ | Kind _ | Constraint _ | Manifest _
      | Parameter_jkind _ | Private_variant _ | Private_object _ | Variance
      | Record_mismatch
          ( Inlined_representation _ | Float_representation _
          | Ufloat_representation _ | Mixed_representation _
          | Mixed_representation_with_flat_floats _
          | Representation_shape_mismatch )
      | Unboxed_representation _ | Extensible_representation _
      | With_null_representation _ | Fixed_representation _ | Jkind _ )) ->
    []

module Inclusion = struct
  open Includemod.Error

  type context =
    { env : Includemod.Functor_inclusion_diff.inclusion_env;
      fallback : Location.t;
      sides : sides;
      reported_loc : Location.t;
      orientation : Orientation.t
    }

  let reversed (ctx : context) =
    { ctx with orientation = Orientation.reverse ctx.orientation }

  let mode_stories ?expected_declaration (ctx : context) pinpoint error =
    Mode_diagnostics.mode_error_stories ~error_loc:ctx.reported_loc
      ?expected_declaration pinpoint error

  let rec stories_of_all (ctx : context) (all : all) : story list =
    match all with
    | In_Compilation_unit (_, { symptom; _ }) ->
      stories_of_signature ctx symptom
    | In_Signature s | In_Include_functor_signature s ->
      stories_of_signature ctx s
    | In_Module_type d -> stories_of_module_type_diff ctx d
    | In_Module_type_substitution (_, { symptom; _ }) ->
      stories_of_mtd_symptom ctx symptom
    | In_Type_declaration (id, c) | In_Jkind_declaration (id, c) ->
      stories_of_core ctx id c
    | In_Expansion _ -> []

  and stories_of_module_type_diff (ctx : context)
      ({ symptom; _ } : module_type_diff) =
    stories_of_module_type_symptom ctx symptom

  and stories_of_module_type_symptom (ctx : context)
      (symptom : module_type_symptom) =
    match symptom with
    | Mt_core _ | Invalid_module_alias _ -> []
    | Signature s -> stories_of_signature ctx s
    | Functor (Params ({ got; expected; _ } as diff)) -> begin
      let outer_expected, outer_got =
        Orientation.expected_and_actual ctx.orientation ~got ~expected
      in
      match outer_got.params, outer_expected.params with
      | [], _ | _, [] -> []
      | _ :: _, _ :: _ -> stories_of_functor_params ctx diff
      end
    | Functor (Result d) -> stories_of_module_type_diff ctx d
    | After_alias_expansion d -> stories_of_module_type_diff ctx d
    | Mode e -> mode_stories ctx (ctx.fallback, Mode.Hint.Module) e

  and stories_of_functor_params (ctx : context)
      ({ got; expected; _ } : functor_params_diff) =
    let patch =
      Includemod.Functor_inclusion_diff.diff ctx.env (got.params, got.res)
        (expected.params, expected.res)
    in
    let numbered = match patch with [] | [_] -> false | _ :: _ :: _ -> true in
    List.concat
      (List.mapi
         (fun index change ->
           match (change : _ Diffing.change) with
           | Diffing.Keep _ -> []
           | Diffing.Change (_, _, Mismatch d) -> begin
             match stories_of_module_type_diff (reversed ctx) d with
             | [] -> []
             | children ->
               [ parameters_do_not_match
                   ~position:(if numbered then Some (index + 1) else None)
                   children ]
             end
           | Diffing.Change (_, _, Incompatible_params _)
           | Diffing.Insert _ | Diffing.Delete _ ->
             [])
         patch)

  and stories_of_signature (ctx : context)
      ({ env; subst; missings = _; incompatibles } : signature_symptom) =
    let ctx =
      { ctx with
        env = { Includemod.Functor_inclusion_diff.i_env = env; i_subst = subst }
      }
    in
    List.concat_map
      (fun (id, symptom) -> stories_of_sigitem ctx id symptom)
      incompatibles

  and stories_of_sigitem (ctx : context) id (symptom : sigitem_symptom) =
    match symptom with
    | Core c -> stories_of_core ctx id c
    | Module_type_declaration { got; expected; symptom } -> begin
      match stories_of_mtd_symptom ctx symptom with
      | [] -> []
      | children ->
        let expected_loc, got_loc =
          Orientation.expected_and_actual ctx.orientation
            ~got:(Some got.Types.mtd_loc)
            ~expected:(Some expected.Types.mtd_loc)
        in
        [ declarations_do_not_match ~noun:"module type " ~name:(Ident.name id)
            ~expected_loc ~got_loc
            ~background:
              [ Nlg.background
                  [ Nlg.txt
                      "module type declarations must be equal on both sides" ]
              ]
            children ]
      end
    | Module_type d -> begin
      match stories_of_module_type_diff ctx d with
      | [] -> []
      | children ->
        [ declarations_do_not_match ~noun:"module " ~name:(Ident.name id)
            ~expected_loc:None ~got_loc:None children ]
      end

  and stories_of_mtd_symptom (ctx : context)
      (symptom : module_type_declaration_symptom) =
    let direction ctx ~not_included d =
      match stories_of_module_type_diff ctx d with
      | [] -> []
      | children -> [is_not_included_in ~sides:ctx.sides ~not_included children]
    in
    let got_not_included = Orientation.got_side ctx.orientation in
    let expected_not_included = Orientation.expected_side ctx.orientation in
    match symptom with
    | Illegal_permutation _ -> []
    | Not_less_than d -> direction ctx ~not_included:got_not_included d
    | Not_greater_than d ->
      direction (reversed ctx) ~not_included:expected_not_included d
    | Incomparable { less_than; greater_than } -> begin
      match
        ( direction ctx ~not_included:got_not_included less_than,
          direction (reversed ctx) ~not_included:expected_not_included
            greater_than )
      with
      | [first], [second] when same_children first second ->
        [Nlg.beheaded first]
      | first, second -> first @ second
      end

  and stories_of_core (ctx : context) id (symptom : core_sigitem_symptom) =
    match symptom with
    | Value_descriptions { got; expected; symptom = Includecore.Mode e } ->
      mode_stories ~expected_declaration:expected ctx
        (got.Types.val_loc, Mode.Hint.Structure_item (Mode.Hint.Value, id))
        e
    | Class_declarations { symptom = Class_mode e; _ } ->
      mode_stories ctx (ctx.fallback, Mode.Hint.Class) e
    | Value_descriptions { got; expected; symptom = Includecore.Modality e; _ }
      ->
      [ Mode_diagnostics.modality_story ~sides:ctx.sides
          (modality_input ~orientation:ctx.orientation
             ~subject:(Modality_item (Ident.name id))
             ~expected_loc:(Some expected.Types.val_loc)
             ~actual_loc:(Some got.Types.val_loc)
             ~requirement:At_least_as_strong e) ]
    | Modalities e ->
      [ Mode_diagnostics.modality_story ~sides:ctx.sides
          (modality_input ~orientation:ctx.orientation
             ~subject:(Modality_item (Ident.name id))
             ~expected_loc:None ~actual_loc:None
             ~requirement:At_least_as_strong e) ]
    | Type_declarations { got; expected; symptom } -> begin
      let expected_loc, got_loc =
        Orientation.expected_and_actual ctx.orientation
          ~got:(Some got.Types.type_loc)
          ~expected:(Some expected.Types.type_loc)
      in
      match
        type_stories ~sides:ctx.sides ~orientation:ctx.orientation
          ~expected_loc ~actual_loc:got_loc symptom
      with
      | [] -> []
      | children ->
        [ declarations_do_not_match ~noun:"type " ~name:(Ident.name id)
            ~expected_loc ~got_loc children ]
      end
    | Extension_constructors
        { got;
          expected;
          symptom = Includecore.Constructor_mismatch (_, ext1, ext2, reason)
        } -> begin
      let children =
        match (reason : Includecore.constructor_mismatch) with
        | Includecore.Modality (i, equate) ->
          [ Mode_diagnostics.modality_story ~sides:ctx.sides
              (equate_modality_input ~orientation:ctx.orientation
                 ~subject:
                   (Modality_constructor_arg
                      { constructor = Ident.name id; index = i + 1 })
                 ~expected_loc:(Some ext2.Types.ext_loc)
                 ~actual_loc:(Some ext1.Types.ext_loc) equate) ]
        | Includecore.Inline_record changes ->
          field_stories ~sides:ctx.sides ~orientation:ctx.orientation changes
        | Includecore.(
            ( Type _ | Arity | Kind _ | Explicit_return_type _
            | Fixed_representation _ | Immediate_representation _
            | Constructor_representation_shape_mismatch )) ->
          []
      in
      match children with
      | [] -> []
      | children ->
        let expected_loc, got_loc =
          Orientation.expected_and_actual ctx.orientation
            ~got:(Some got.Types.ext_loc)
            ~expected:(Some expected.Types.ext_loc)
        in
        [ declarations_do_not_match
            ~noun:
              (if Path.same got.Types.ext_type_path Predef.path_exn
               then "exception "
               else "the constructor ")
            ~name:(Ident.name id) ~expected_loc ~got_loc children ]
      end
    | Value_descriptions { symptom = Includecore.Zero_alloc _; _ } -> []
    | Value_descriptions
        { symptom =
            Includecore.(
              ( Primitive_mismatch _ | Not_a_primitive | Type _
              | Layout_poly_coercion _ ));
          _
        } ->
      []
    | Class_declarations { symptom = Class_type _; _ }
    | Extension_constructors { symptom = Includecore.Constructor_privacy; _ }
    | Class_type_declarations _ | Jkind_declarations _ ->
      []

  let initial ~i_env ~fallback ~sides ~reported_loc =
    { env =
        { Includemod.Functor_inclusion_diff.i_env; i_subst = Subst.identity };
      fallback;
      sides;
      reported_loc;
      orientation = Orientation.Got_is_actual
    }

  let stories ~sides ~fallback ~reported_loc
      ((i_env, all) : Includemod.explanation) =
    stories_of_all (initial ~i_env ~fallback ~sides ~reported_loc) all

  let module_type_stories ~env ~sides ~reported_loc symptom =
    stories_of_module_type_symptom
      (initial ~i_env:env ~fallback:reported_loc ~sides ~reported_loc)
      symptom
end

let rec leftmost_functor (lid : Longident.t) : Longident.t =
  match lid with
  | Lapply (f, _) -> leftmost_functor f.txt
  | (Lident _ | Ldot _) as lid -> lid

let diagnose ~reported_loc (error : error) =
  match error with
  | Not_included { loc; explanation } ->
    let subject, predicate =
      match explanation with
      | _, Includemod.Error.In_Compilation_unit (_, { got; _ }) ->
        ( Nlg.subject [Phrase.Text "module "; Phrase.Code got],
          [Nlg.txt " does not match its interface"] )
      | ( _,
          Includemod.Error.(
            ( In_Signature _ | In_Include_functor_signature _
            | In_Module_type _ | In_Module_type_substitution _
            | In_Type_declaration _ | In_Jkind_declaration _
            | In_Expansion _ )) ) ->
        ( Nlg.subject ~span:reported_loc [Phrase.Text "the module"],
          [Nlg.txt " does not match its signature"] )
    in
    frame ~subject predicate
      (Inclusion.stories ~sides:declaration_sides ~fallback:loc ~reported_loc
         explanation)
  | Strengthening_mismatch { loc; path = lid; explanation } ->
    let sides =
      { expected_name = [Nlg.txt "the module type"];
        actual_name = [Nlg.txt "the module"]
      }
    in
    let subject =
      Nlg.subject ~span:reported_loc
        (match Nlg.longident_name lid with
        | Some name -> [Phrase.Text "module "; Phrase.Code name]
        | None -> [Phrase.Text "the strengthening module"])
    in
    frame ~subject
      [Nlg.txt " does not match the module type it strengthens"]
      (Inclusion.stories ~sides ~fallback:loc ~reported_loc explanation)
  | Applicative_functor_mismatch
      { loc; constrained = lid; type_path = path; explanation } ->
    let sides =
      { expected_name = [Nlg.txt "the functor's parameter"];
        actual_name = [Nlg.txt "the module after substitution"]
      }
    in
    let subject =
      Nlg.subject ~span:reported_loc
        [Phrase.Text "the type "; Phrase.Code (Path.name path)]
    in
    let predicate =
      match Nlg.longident_name lid with
      | Some name ->
        [ Nlg.txt " is ill-typed after this ";
          Nlg.code "with";
          Nlg.txt " constraint on ";
          Nlg.code name ]
      | None ->
        [ Nlg.txt " is ill-typed after this ";
          Nlg.code "with";
          Nlg.txt " constraint" ]
    in
    frame ~subject predicate
      (Inclusion.stories ~sides ~fallback:loc ~reported_loc explanation)
  | Substitution_mismatch { loc; path = lid; explanation } ->
    let sides =
      { expected_name = [Nlg.txt "the new definition"];
        actual_name = [Nlg.txt "the original definition"]
      }
    in
    let subject =
      Nlg.subject ~span:reported_loc
        (match Nlg.longident_name lid with
        | Some name -> [Phrase.Text "the new definition of "; Phrase.Code name]
        | None -> [Phrase.Text "the new definition"])
    in
    frame ~subject
      [Nlg.txt " does not match its original definition"]
      (Inclusion.stories ~sides ~fallback:loc ~reported_loc explanation)
  | Functor_application_mismatch { env; app_name; mty_f; args } -> begin
    let failing =
      List.filter_map
        (fun change ->
          match change with
          | Diffing.Change ((descr, _, _), _, Includemod.Error.Mismatch d) ->
            Some (descr, d)
          | Diffing.Change (_, _, Includemod.Error.Incompatible_params _)
          | Diffing.Delete _ | Diffing.Insert _ | Diffing.Keep _ ->
            None)
        (Includemod.Functor_app_diff.diff env ~f:mty_f ~args)
    in
    match failing with
    | [] -> []
    | failing ->
      let subject =
        let argument =
          match failing with
          | [(Includemod.Error.Named path, _)] -> Some (Path.name path)
          | _ -> None
        in
        Nlg.subject ~span:reported_loc
          (match argument with
          | Some argument ->
            [Phrase.Text "the argument "; Phrase.Code argument]
          | None -> [Phrase.Text "the argument"])
      in
      let predicate =
        let functor_name =
          match (app_name : Includemod.application_name) with
          | Anonymous_functor -> None
          | Named_leftmost_functor lid -> Nlg.longident_name lid
          | Full_application_path lid ->
            Nlg.longident_name (leftmost_functor lid)
        in
        match functor_name with
        | Some functor_name ->
          [ Nlg.txt " does not match the parameter of ";
            Nlg.code functor_name ]
        | None -> [Nlg.txt " does not match the functor's parameter"]
      in
      let sides =
        { expected_name = [Nlg.txt "the parameter"];
          actual_name = [Nlg.txt "the argument"]
        }
      in
      frame ~subject predicate
        (List.concat_map
           (fun (_, (diff : Includemod.Error.module_type_diff)) ->
             Inclusion.module_type_stories ~env ~sides ~reported_loc
               diff.symptom)
           failing)
    end
  | Type_definition_mismatch { loc; type_expr; env; mismatch } -> begin
    match Types.get_desc type_expr with
    | Types.Tconstr (path, _, _) ->
      let name = Path.name path in
      let equated_loc =
        match Env.find_type path env with
        | (decl : Types.type_declaration) -> Some decl.type_loc
        | exception Not_found -> None
      in
      let sides =
        { expected_name = [Nlg.txt "this definition"];
          actual_name = [Nlg.txt "the definition of "; Nlg.code name]
        }
      in
      let subject =
        Nlg.subject ~span:reported_loc [Phrase.Text "this definition"]
      in
      let predicate =
        Nlg.txt " does not match the definition of "
        :: located equated_loc [Nlg.code name]
      in
      frame ~subject predicate
        (type_stories ~sides ~orientation:Orientation.Got_is_actual
           ~expected_loc:(Some loc) ~actual_loc:equated_loc mismatch)
    | Types.Tvar _ | Types.Tarrow _ | Types.Ttuple _ | Types.Tunboxed_tuple _
    | Types.Tobject _ | Types.Tfield _ | Types.Tquote _ | Types.Tsplice _
    | Types.Tquote_eval _ | Types.Tnil | Types.Tlink _ | Types.Tsubst _
    | Types.Tvariant _ | Types.Tunivar _ | Types.Tpoly _ | Types.Trepr _
    | Types.Tpackage _ | Types.Tof_kind _ | Types.Tmod _ | Types.Tbox _ ->
      []
    end

let diagnose ~loc error =
  Diagnostic_term.diagnose ~loc (fun () -> diagnose ~reported_loc:loc error)
