module Diagnostic = Structured_diagnostic
module Nlg = Diagnostic_nlg
module Phrase = Nlg.Phrase

type inclusion_site =
  | Module of
      { name : string option;
        body : Location.t
      }
  | Module_type of
      { name : string option;
        body : Location.t
      }

type frame =
  | Compilation_unit of string
  | Inclusion_site of inclusion_site
  | Substitution of string option
  | Applicative_functor of string * string option
  | Strengthening of string option
  | Application of string option * string option
  | Equation of string * Location.t option
  | Unknown

type direction =
  | Actual_not_included
  | Expected_not_included

type item =
  | Item_module of string
  | Item_module_type of string
  | Item_type of string
  | Item_extension_constructor of
      { exception_ : bool;
        name : string
      }
  | Item_functor_parameter of int option
  | Direction of direction

type 'term sides =
  { expected_name : 'term Phrase.segment list;
    actual_name : 'term Phrase.segment list
  }

let declaration_sides () =
  { expected_name = [Nlg.txt "the expected declaration"];
    actual_name = [Nlg.txt "the actual declaration"]
  }

let subject ?span name = { Nlg.name; span }

let sentence_subject (subject : Nlg.subject) =
  Option.map (fun (_ : Location.t) -> subject) subject.span

let claims plans =
  List.map (fun plan -> Diagnostic.Relation.Claim, plan) plans

let elaboration sentence =
  Diagnostic.Relation.Elaboration, Nlg.Plan.statement sentence

let ordinal n =
  let suffix =
    let mod100 = n mod 100 in
    if mod100 >= 11 && mod100 <= 13
    then "th"
    else match n mod 10 with 1 -> "st" | 2 -> "nd" | 3 -> "rd" | _ -> "th"
  in
  string_of_int n ^ suffix

let inclusion_frame ~loc frame =
  let named ?span words = subject ?span words in
  let subject, predicate =
    match frame with
    | Compilation_unit name ->
      ( named [Phrase.Text "module "; Phrase.Code name],
        [Nlg.txt " does not match its interface"] )
    | Inclusion_site (Module { name = Some name; body }) ->
      ( named ~span:body [Phrase.Text "module "; Phrase.Code name],
        [Nlg.txt " does not match its signature"] )
    | Inclusion_site (Module { name = None; body }) ->
      ( named ~span:body [Phrase.Text "the anonymous module"],
        [Nlg.txt " does not match its signature"] )
    | Inclusion_site (Module_type { name = Some name; body }) ->
      ( named ~span:body [Phrase.Text "the module type "; Phrase.Code name],
        [Nlg.txt " does not match its declaration"] )
    | Inclusion_site (Module_type { name = None; body }) ->
      ( named ~span:body [Phrase.Text "the anonymous module type"],
        [Nlg.txt " does not match its declaration"] )
    | Substitution name ->
      ( named ~span:loc
          (match name with
          | Some name ->
            [Phrase.Text "the new definition of "; Phrase.Code name]
          | None -> [Phrase.Text "the new definition"]),
        [Nlg.txt " does not match its original definition"] )
    | Applicative_functor (type_name, constrained) ->
      ( named ~span:loc [Phrase.Text "the type "; Phrase.Code type_name],
        match constrained with
        | Some name ->
          [ Nlg.txt " is ill-typed after this ";
            Nlg.code "with";
            Nlg.txt " constraint on ";
            Nlg.code name ]
        | None ->
          [ Nlg.txt " is ill-typed after this ";
            Nlg.code "with";
            Nlg.txt " constraint" ] )
    | Strengthening name ->
      ( named ~span:loc
          (match name with
          | Some name -> [Phrase.Text "module "; Phrase.Code name]
          | None -> [Phrase.Text "the strengthening module"]),
        [Nlg.txt " does not match the module type it strengthens"] )
    | Application (functor_name, argument) ->
      ( named ~span:loc
          (match argument with
          | Some argument -> [Phrase.Text "the argument "; Phrase.Code argument]
          | None -> [Phrase.Text "the argument"]),
        match functor_name with
        | Some functor_name ->
          [Nlg.txt " does not match the parameter of "; Nlg.code functor_name]
        | None -> [Nlg.txt " does not match the functor's parameter"] )
    | Equation (name, equated_loc) ->
      ( named ~span:loc [Phrase.Text "this definition"],
        Nlg.txt " does not match the definition of "
        ::
        (match equated_loc with
        | Some loc -> [Nlg.ref_source loc [Nlg.code name]]
        | None -> [Nlg.code name]) )
    | Unknown ->
      ( named ~span:loc [Phrase.Text "the module"],
        [Nlg.txt " does not match its signature"] )
  in
  Nlg.Plan.statement
    (Nlg.sentence ?subject:(sentence_subject subject)
       (Nlg.mention ~case:Subject subject :: predicate))

let item_frame ~sides item ~got_loc ~expected_loc ~children =
  let spans = List.filter_map Fun.id [expected_loc; got_loc] in
  let named noun name =
    let span = match spans with span :: _ -> Some span | [] -> None in
    let subject = subject ?span [Phrase.Text noun; Phrase.Code name] in
    Nlg.sentence ?subject:(sentence_subject subject)
      [ Nlg.txt "the declarations of ";
        Nlg.mention ~case:Subject subject;
        Nlg.txt " do not match" ]
  in
  let header =
    match item with
    | Item_module name -> named "module " name
    | Item_module_type name -> named "module type " name
    | Item_type name -> named "type " name
    | Item_extension_constructor { exception_; name } ->
      named (if exception_ then "exception " else "the constructor ") name
    | Item_functor_parameter None ->
      Nlg.sentence [Nlg.txt "the functors' parameters do not match"]
    | Item_functor_parameter (Some position) ->
      Nlg.sentence
        [ Nlg.txt
            ("the declarations of the " ^ ordinal position
           ^ " parameter do not match") ]
    | Direction direction ->
      let not_included, container =
        match direction with
        | Actual_not_included -> sides.actual_name, sides.expected_name
        | Expected_not_included -> sides.expected_name, sides.actual_name
      in
      Nlg.sentence
        (not_included @ (Nlg.txt " is not included in " :: container))
  in
  let background =
    match item with
    | Item_module_type _ ->
      [ elaboration
          (Nlg.sentence ~kind:Diagnostic.Kind.Background
             [Nlg.txt "module type declarations must be equal on both sides"])
      ]
    | Item_module _ | Item_type _ | Item_extension_constructor _
    | Item_functor_parameter _ | Direction _ ->
      []
  in
  Nlg.Plan.statement ~children:(claims children @ background) header
