(* Library unit A: param-heavy and with-bound decls whose ikinds are stored in
   liba.cmi and consumed cross-unit by the client.  Regression guard for
   stage-4d: decl ikinds persist across the cmi boundary and are consumed
   consistently under -ikinds. *)
type 'a box : immutable_data with 'a = Box of 'a
type ('a, 'b) pair : immutable_data with 'a * 'b = { fst : 'a; snd : 'b }
type 'a nested : immutable_data with 'a box = Nested of 'a box
