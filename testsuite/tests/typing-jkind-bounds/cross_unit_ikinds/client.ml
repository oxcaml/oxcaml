(* Client unit B: consumes A's decls in crossing/subsumption contexts, exercising
   A's stored (persisted) ikinds. *)
type ib = int Liba.box
type ip = (int, int) Liba.pair
type crosser : immutable_data = { a : int Liba.box; b : (int, int) Liba.pair }
type 'a re : immutable_data with 'a Liba.box = Re of 'a Liba.nested
