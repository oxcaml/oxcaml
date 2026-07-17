(* TEST
 flags = " -w -a ";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A large `module rec` group of independent members (no manifest cycles),
   accepted at unroll depth 1. Guards accept behaviour on the depth-1 path. *)

module rec M0 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M1 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M2 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M3 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M4 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M5 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M6 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M7 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M8 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M9 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M10 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M11 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M12 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M13 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M14 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M15 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M16 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M17 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M18 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M19 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M20 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M21 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M22 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M23 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M24 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M25 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M26 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M27 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M28 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M29 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M30 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M31 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M32 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M33 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M34 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M35 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M36 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M37 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M38 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M39 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M40 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M41 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M42 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M43 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M44 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M45 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M46 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M47 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M48 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M49 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M50 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M51 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M52 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M53 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M54 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M55 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M56 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M57 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M58 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M59 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M60 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M61 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M62 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M63 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M64 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M65 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M66 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M67 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M68 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M69 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M70 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
and M71 : sig type t = { a : int; b : string } val make : int -> string -> t end
  = struct type t = { a : int; b : string } let make a b = { a; b } end
