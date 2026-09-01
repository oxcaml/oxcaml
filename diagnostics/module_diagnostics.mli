module Nlg = Diagnostic_nlg

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
  { expected_name : 'term Nlg.Phrase.segment list;
    actual_name : 'term Nlg.Phrase.segment list
  }

val declaration_sides : unit -> 'term sides

val inclusion_frame :
  loc:Location.t -> frame -> 'term Nlg.plan

val item_frame :
  sides:'term sides ->
  item ->
  got_loc:Location.t option ->
  expected_loc:Location.t option ->
  children:'term Nlg.plan list ->
  'term Nlg.plan
