(* TEST
 expect;
*)

(* Unboxed record contained in another unboxed record. *)
module M_urecord_urecord : sig
  type line = #{ p : point ; q : point }
  and point = #{ i : int ; j : int }
end = struct
  type point = #{ i : int ; j : int }
  type line = #{ p : point ; q : point }
end
[%%expect{|
module M_urecord_urecord :
  sig
    type line = #{ p : point; q : point; }
    and point = #{ i : int; j : int; }
  end
|}]

(* Boxed variant outside, unboxed record inside. *)
module M_variant_urecord : sig
  type t = A of u
  and u = #{ x : int ; y : int }
end = struct
  type u = #{ x : int ; y : int }
  type t = A of u
end
[%%expect{|
module M_variant_urecord :
  sig type t = A of u and u = #{ x : int; y : int; } end
|}]

(* Boxed variant outside, unboxed version of a boxed record inside. *)
module M_variant_uversion : sig
  type t = A of u#
  and u = { x : int ; y : int }
end = struct
  type u = { x : int ; y : int }
  type t = A of u#
end
[%%expect{|
module M_variant_uversion :
  sig type t = A of u# and u = { x : int; y : int; } end
|}]

(* Boxed record outside, unboxed record inside. *)
module M_record_urecord : sig
  type t = { f : u }
  and u = #{ x : int ; y : int }
end = struct
  type u = #{ x : int ; y : int }
  type t = { f : u }
end
[%%expect{|
module M_record_urecord :
  sig type t = { f : u; } and u = #{ x : int; y : int; } end
|}]

(* Boxed record outside, unboxed version of a boxed record inside. *)
module M_record_uversion : sig
  type t = { f : u# }
  and u = { x : int ; y : int }
end = struct
  type u = { x : int ; y : int }
  type t = { f : u# }
end
[%%expect{|
module M_record_uversion :
  sig type t = { f : u#; } and u = { x : int; y : int; } end
|}]

(* A longer chain through several unboxed records. *)
module M_chain : sig
  type a = #{ x : b ; y : b }
  and b = #{ x : c ; y : c }
  and c = #{ i : int ; j : int }
end = struct
  type c = #{ i : int ; j : int }
  type b = #{ x : c ; y : c }
  type a = #{ x : b ; y : b }
end
[%%expect{|
module M_chain :
  sig
    type a = #{ x : b; y : b; }
    and b = #{ x : c; y : c; }
    and c = #{ i : int; j : int; }
  end
|}]

(* The contained type can be hidden behind an abbreviation, which we expand
   rather than approximate by layout (so the abbreviation's own layout
   annotation, [value & value] here, is irrelevant). *)
type ('a : value & value) id = 'a
[%%expect{|
type ('a : value & value) id = 'a
|}]

module M_abbrev : sig
  type t = A of u# id
  and u = { x : int ; y : int }
end = struct
  type u = { x : int ; y : int }
  type t = A of u# id
end
[%%expect{|
module M_abbrev : sig type t = A of u# id and u = { x : int; y : int; } end
|}]

(* Same, with an [any]-layout abbreviation. *)
type ('a : any) id = 'a
[%%expect{|
type ('a : any) id = 'a
|}]

module M_abbrev_any : sig
  type t = A of u# id
  and u = { x : int ; y : int }
end = struct
  type u = { x : int ; y : int }
  type t = A of u# id
end
[%%expect{|
module M_abbrev_any :
  sig type t = A of u# id and u = { x : int; y : int; } end
|}]

(* An unboxed record may contain a boxed variant that in turn contains an
   unboxed record: [x] does not depend on the boxed [t] (a value), but [t] still
   depends on [u]. *)
module M_through_box : sig
  type x = #{ f : t ; g : int }
  and t = A of u
  and u = #{ a : int ; b : int }
end = struct
  type u = #{ a : int ; b : int }
  type t = A of u
  type x = #{ f : t ; g : int }
end
[%%expect{|
module M_through_box :
  sig
    type x = #{ f : t; g : int; }
    and t = A of u
    and u = #{ a : int; b : int; }
  end
|}]
