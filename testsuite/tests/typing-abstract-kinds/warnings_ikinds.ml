(* TEST
   flags = "-w +191";
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
