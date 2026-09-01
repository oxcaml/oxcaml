module Pronouns = struct
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

let create ~source ~context ~pronouns ~reported_loc =
  { source; context; pronouns; reported_loc }
