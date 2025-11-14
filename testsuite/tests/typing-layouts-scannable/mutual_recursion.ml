(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

type t_maybeptr_val : value maybe_pointer
type t_nonptr_val : value non_pointer
[%%expect{|
type t_maybeptr_val
type t_nonptr_val : value non_pointer
|}]

(* CR layouts-scannable: The current approach does not play nicely with
   mutually recursive declarations, as demonstrated by the following test: *)

type a : (value non_pointer & value) & (value non_pointer & value non_pointer)
       = #{ p : #(t_nonptr_val * t_maybeptr_val); b : b }
and b = #{ i : t_nonptr_val; j : t_nonptr_val }
[%%expect{|
Lines 1-2, characters 0-57:
1 | type a : (value non_pointer & value) & (value non_pointer & value non_pointer)
2 |        = #{ p : #(t_nonptr_val * t_maybeptr_val); b : b }
Error: The layout of type "a" is (value non_pointer & value) & (value & value)
         because it is an unboxed record.
       But the layout of type "a" must be a sublayout of
           (value non_pointer & value)
           & (value non_pointer & value non_pointer)
         because of the annotation on the declaration of the type a.
|}]

(* BUT adding in the additional kind annotation makes this work! *)
type a : (value non_pointer & value) & (value non_pointer & value non_pointer)
       = #{ p : #(t_nonptr_val * t_maybeptr_val); b : b }
and b : value non_pointer & value non_pointer
      = #{ i : t_nonptr_val; j : t_nonptr_val }
[%%expect{|
type a = #{ p : #(t_nonptr_val * t_maybeptr_val); b : b; }
and b = #{ i : t_nonptr_val; j : t_nonptr_val; }
|}]

type a : (value non_pointer & value) & (value non_pointer & value non_pointer)
       = #{ p : #(t_nonptr_val * t_maybeptr_val);
            b : (b as (_ : value non_pointer & value non_pointer)) }
and b = #{ i : t_nonptr_val; j : t_nonptr_val }
[%%expect{|
Lines 1-3, characters 0-68:
1 | type a : (value non_pointer & value) & (value non_pointer & value non_pointer)
2 |        = #{ p : #(t_nonptr_val * t_maybeptr_val);
3 |             b : (b as (_ : value non_pointer & value non_pointer)) }
Error: The layout of type "a" is (value non_pointer & value) & (value & value)
         because it is an unboxed record.
       But the layout of type "a" must be a sublayout of
           (value non_pointer & value)
           & (value non_pointer & value non_pointer)
         because of the annotation on the declaration of the type a.
|}]

(* These almost demonstrate the bad mutual recursion behavior, but work. *)

type a : (value non_pointer & value) & value non_pointer
       = #{ p : #(t_nonptr_val * t_maybeptr_val); b : b }
and b : value maybe_pointer = #{ i : t_nonptr_val }
[%%expect{|
type a = #{ p : #(t_nonptr_val * t_maybeptr_val); b : b; }
and b = #{ i : t_nonptr_val; }
|}]

type a : (value non_pointer & value) & value non_pointer
       = #{ p : #(t_nonptr_val2 * t_maybeptr_val); b : b }
and t_nonptr_val2 = #{ i : t_nonptr_val }
[%%expect{|
type a = #{ p : #(t_nonptr_val2 * t_maybeptr_val); b : b; }
and t_nonptr_val2 = #{ i : t_nonptr_val; }
|}]

module M : sig
  type a : (value non_pointer & value) & value non_pointer
  and b : value non_pointer
end = struct
  type a = #{ p : #(t_nonptr_val * t_maybeptr_val); b : b }
  and b = #{ i : t_nonptr_val }
end
[%%expect{|
module M :
  sig
    type a : (value non_pointer & value) & value non_pointer
    and b : value non_pointer
  end
|}]
