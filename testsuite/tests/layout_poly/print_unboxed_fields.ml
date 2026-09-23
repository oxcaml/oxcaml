(* TEST
 flags = "-extension layout_poly_alpha -extension layouts_beta";
 ocamlrunparam += ",s=4k";
 { expect; }
 { expect.opt; }
*)

(* Printing values whose layout-polymorphic fields hold unboxed values. The
   printer must never treat the raw bits of such a field as a value: the
   small minor heap requested above makes a GC during printing likely, which
   used to crash the native toplevel when a raw float sat in a value-typed
   variable. Fields declared after an unboxed field also check that fields
   are read at their runtime position, which differs from the declaration
   order in mixed blocks. *)

type ('a : any) record = { x : int; y : 'a }
type ('a : any) reordered_record = { y : 'a; x : int }

let record =
  let poly_ mk y : _ record = { x = 67; y } in
  mk 42, mk #42.5
[%%expect{|
type ('a : any) record = { x : int; y : 'a; }
type ('a : any) reordered_record = { y : 'a; x : int; }
val record : int record * float# record =
  ({x = 67; y = 42}, {x = 67; y = <abstr>})
|}]

let reordered_record =
  let poly_ mk y : _ reordered_record = { y; x = 67 } in
  mk 42, mk #42.5
[%%expect{|
val reordered_record : int reordered_record * float# reordered_record =
  ({y = 42; x = 67}, {y = <abstr>; x = 67})
|}]

type ('a : any) inline_record = I of { x : int; y : 'a }
type ('a : any) reordered_inline_record = J of { y : 'a; x : int }

let inline_record =
  let poly_ mk y = I { x = 67; y } in
  mk 42, mk #42.5
[%%expect{|
type ('a : any) inline_record = I of { x : int; y : 'a; }
type ('a : any) reordered_inline_record = J of { y : 'a; x : int; }
val inline_record : int inline_record * float# inline_record =
  (I {x = 67; y = 42}, I {x = 67; y = <abstr>})
|}]

let reordered_inline_record =
  let poly_ mk y = J { y; x = 67 } in
  mk 42, mk #42.5
[%%expect{|
val reordered_inline_record :
  int reordered_inline_record * float# reordered_inline_record =
  (J {y = 42; x = 67}, J {y = <abstr>; x = 67})
|}]

type ('a : any) constructor = C of int * 'a
type ('a : any) reordered_constructor = D of 'a * int

let constructor =
  let poly_ mk y = C (67, y) in
  mk 42, mk #42.5
[%%expect{|
type ('a : any) constructor = C of int * 'a
type ('a : any) reordered_constructor = D of 'a * int
val constructor : int constructor * float# constructor =
  (C (67, 42), C (67, <abstr>))
|}]

let reordered_constructor =
  let poly_ mk y = D (y, 67) in
  mk 42, mk #42.5
[%%expect{|
val reordered_constructor :
  int reordered_constructor * float# reordered_constructor =
  (D (42, 67), D (<abstr>, 67))
|}]

(* Several unboxed fields around value fields. *)
type ('a : any, 'b : any) many = { a : 'a; x : int; b : 'b; y : string }

let many =
  let poly_ mk a b = { a; x = 67; b; y = "y" } in
  mk #1.5 #4.5, mk 42 #4.5, mk #1.5 42
[%%expect{|
type ('a : any, 'b : any) many = { a : 'a; x : int; b : 'b; y : string; }
val many : (float#, float#) many * (int, float#) many * (float#, int) many =
  ({a = <abstr>; x = 67; b = <abstr>; y = "y"},
   {a = 42; x = 67; b = <abstr>; y = "y"},
   {a = <abstr>; x = 67; b = 42; y = "y"})
|}]

(* Several values to print, to give a minor collection more chances to happen
   while a field is being printed. *)
let list =
  let poly_ mk y : _ record = { x = 67; y } in
  List.init 8 (fun _ -> mk #42.5)
[%%expect{|
val list : float# record list =
  [{x = 67; y = <abstr>}; {x = 67; y = <abstr>}; {x = 67; y = <abstr>};
   {x = 67; y = <abstr>}; {x = 67; y = <abstr>}; {x = 67; y = <abstr>};
   {x = 67; y = <abstr>}; {x = 67; y = <abstr>}]
|}]
