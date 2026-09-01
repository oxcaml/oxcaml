module Pronouns : sig
  type t =
    | Use_pronouns
    | Names_only
end

type 'context t =
  { source : Diagnostic_source.t;
    context : 'context;
    pronouns : Pronouns.t;
    reported_loc : Location.t
  }

val create :
  source:Diagnostic_source.t ->
  context:'context ->
  pronouns:Pronouns.t ->
  reported_loc:Location.t ->
  'context t
