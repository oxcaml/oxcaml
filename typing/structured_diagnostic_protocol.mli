type json_string = string

val protocol_version : int

val string_to_json : string -> json_string

module Form : sig
  type t =
    | Name
    | Pronoun

  val to_string : t -> string
end

module Kind : sig
  type t =
    | Explanation
    | Background
    | Suggestion

  val to_string : t -> string
end

module Relation : sig
  type t =
    | Claim
    | Elaboration

  val to_string : t -> string
end

module Generic : sig
  type 'loc annotation =
    | Code
    | Source of 'loc
    | Mention of
        { entity : int;
          form : Form.t
        }
    | Term of int

  and 'loc inline =
    | Text of string
    | Annotated of
        { annotation : 'loc annotation;
          content : 'loc inline list
        }

  and 'loc child =
    { relation : Relation.t;
      block : 'loc block
    }

  and 'loc block =
    { kind : Kind.t;
      content : 'loc inline list;
      children : 'loc child list
    }

  type 'loc entity =
    { id : int;
      loc : 'loc
    }

  type glossary_entry =
    { id : int;
      term : string;
      category : string;
      description : string;
      url : string option
    }

  type 'loc diagnostic =
    { loc : 'loc;
      title : string;
      entities : 'loc entity list;
      glossary : glossary_entry list;
      body : 'loc block list
    }

  type 'loc response =
    { version : int;
      diagnostics : 'loc diagnostic list
    }

  val diagnostic_to_json :
    loc_to_json:('loc -> json_string) -> 'loc diagnostic -> json_string

  val response_to_json :
    loc_to_json:('loc -> json_string) -> 'loc response -> json_string
end

module Raw : sig
  module Position : sig
    type t =
      { line : int;
        col : int
      }

    val to_json : t -> json_string
  end

  module Location : sig
    type t =
      { file : string;
        start : Position.t;
        end_ : Position.t
      }

    val to_json : t -> json_string
  end

  type diagnostic = Location.t Generic.diagnostic

  type response = Location.t Generic.response

  val response_of_diagnostics : diagnostic list -> response

  val diagnostic_to_json : diagnostic -> json_string

  val response_to_json : response -> json_string
end
