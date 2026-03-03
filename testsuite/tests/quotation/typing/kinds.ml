(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

(** Test that quotes in kinds work **)

(* Annotated quote-kinded abstract types *)
type t : <[value]>
[%%expect {|
type t
|}]
type t : <[immediate]>
[%%expect {|
type t : immediate
|}]
type t : <[value mod portable]>
[%%expect {|
Line 1, characters 0-31:
1 | type t : <[value mod portable]>
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error:
       The kind of t is value mod portable
         because of the annotation on the declaration of the type t.
       But the kind of t must be a subkind of any
         because a dummy kind of any is used to check mutually recursive datatypes.
                 Please notify the Jane Street compilers group if you see this output.
|}]
type t : <[<[value]>]>
[%%expect {|
type t
|}]
