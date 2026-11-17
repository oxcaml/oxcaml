module Intf = struct
  type t = unit
end

module Impl = struct
  type t =
    | Block of { tag : int; fields : t list }
    | Field of { block : t; index : int }
    | LetBinding of { bound : t; body : t } (* CR-soon wsturgeon: desugar? *)
    | UpstreamCompilationUnit of { path : string list }
    | Var of { de_bruijn : int }
end
