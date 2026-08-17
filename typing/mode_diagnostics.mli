module Source : sig
  type t

  val create : file:string -> text:string -> t
end

module Documentation : sig
  type t =
    { description : string;
      url : string option
    }

  type lookup =
    { of_mode : Mode.Alloc.atom -> t option;
      of_modality : Mode.Modality.atom -> t option
    }
end

module Pronouns : sig
  type t =
    | Use_pronouns
    | Names_only
end

type inclusion_site =
  | Module of
      { name : string option;
        body : Location.t
      }
  | Module_type of
      { name : string option;
        body : Location.t
      }

type declared_modalities =
  { written : Mode.Modality.atom Location.loc list;
    mutable_implied : Mode.Modality.Const.t
  }

type constructor_argument =
  { argument_type : string;
    argument_loc : Location.t option;
    crossing : Mode.Crossing.t
  }

type context =
  { inclusion_site_at : Location.t -> inclusion_site option;
    declared_modalities_at :
      Location.t -> argument:int option -> declared_modalities option;
    constructor_arguments_at :
      Location.t -> Longident.t option -> constructor_argument list option;
    documentation : Documentation.lookup
  }

val error :
  source:Source.t ->
  context:context ->
  pronouns:Pronouns.t ->
  loc:Location.t ->
  exn ->
  Structured_diagnostic.t option
