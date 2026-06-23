(* TEST
   flags = "-w +44+191";
*)

(* The unused kind decl warning exists *)
module M1 : sig end = struct
  kind_ k
end

(* You can suppress it *)
module M2 : sig end = struct
  kind_ k [@@warning "-191"]
end

module M3 : sig end = struct
  [@@@warning "-191"]
  kind_ k
end

(* Opening a module that shadows a kind triggers the open-shadow warning, just
   as it does for types. *)
module Shadowing = struct
  kind_ k_shadowed = value
  module M = struct kind_ k_shadowed = float64 end
  open M
  type after : k_shadowed = float#
end
