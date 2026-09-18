(* The sibling unit [sibling_dep] depends on: [weak] exports no kind, [strong] is
   annotated. *)
type weak = { id : int }
type strong : immutable_data
