(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

type t_maybeptr_val : value maybe_separable
type t_nonptr_val : value non_pointer
[%%expect{|
type t_maybeptr_val : value maybe_separable
type t_nonptr_val : value non_pointer
|}]

(* CR layouts-scannable: The current approach does not play nicely with
   mutually recursive declarations, as demonstrated by the following test: *)

type a : value non_pointer & value non_pointer = #{ b : b }
and b = #{ i : t_nonptr_val; j : t_nonptr_val }
[%%expect{|
Line 1, characters 0-59:
1 | type a : value non_pointer & value non_pointer = #{ b : b }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "a" is
           value maybe_separable maybe_null
           & value maybe_separable maybe_null
         because it is an unboxed record.
       But the layout of type "a" must be a sublayout of immediate & immediate
         because of the annotation on the declaration of the type a.
|}]

(* BUT adding in the additional kind annotation on [b] makes this work! *)
type a : value non_pointer & value non_pointer = #{ b : b }
and b : value non_pointer & value non_pointer = #{ i : t_nonptr_val; j : t_nonptr_val }
[%%expect{|
type a = #{ b : b; }
and b = #{ i : t_nonptr_val; j : t_nonptr_val; }
|}]

type a : value non_pointer & value non_pointer
       (* BUT an annotation here does not change anything... *)
       = #{ b : (b as (_ : value non_pointer & value non_pointer)) }
and b = #{ i : t_nonptr_val; j : t_nonptr_val }
[%%expect{|
Lines 1-3, characters 0-68:
1 | type a : value non_pointer & value non_pointer
2 |        (* BUT an annotation here does not change anything... *)
3 |        = #{ b : (b as (_ : value non_pointer & value non_pointer)) }
Error: The layout of type "a" is
           value maybe_separable maybe_null
           & value maybe_separable maybe_null
         because it is an unboxed record.
       But the layout of type "a" must be a sublayout of immediate & immediate
         because of the annotation on the declaration of the type a.
|}]

(* Same example as above, split across two recursive modules *)

(* CR layouts: This reports the wrong error message, as the layout of type "a"
   is definitely not [value]. This problem is not unique to scannable axes. *)

module rec M1 : sig
  type a : value non_pointer & value non_pointer = #{ b : M2.b }
end = struct
  type a = #{ b : M2.b; }
end
and M2 : sig
  type b = #{ i : t_nonptr_val; j : t_nonptr_val }
end = struct
  type b = #{ i : t_nonptr_val; j : t_nonptr_val }
end
[%%expect{|
Line 2, characters 2-64:
2 |   type a : value non_pointer & value non_pointer = #{ b : M2.b }
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "a" is value
         because it is an unboxed record.
       But the layout of type "a" must be a sublayout of immediate & immediate
         because of the annotation on the declaration of the type a.
|}]

(* These almost demonstrate the bad mutual recursion behavior, but work. *)

type a : value non_pointer = #{ b : b }
and b : value maybe_separable = #{ i : t_nonptr_val }
[%%expect{|
type a = #{ b : b; }
and b = #{ i : t_nonptr_val; }
|}]

module M : sig
  type a : value non_pointer
  and b : value non_pointer
end = struct
  type a = #{ b : b }
  and b = #{ i : t_nonptr_val }
end
[%%expect{|
module M : sig type a : value non_pointer and b : value non_pointer end
|}]
