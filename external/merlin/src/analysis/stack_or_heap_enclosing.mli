(** Provides information about where allocations around the cursor are placed
    (the stack or the heap).

    The information comes only from the [Mode.Alloc.t] contained in enclosing
    AST nodes.

    - Nodes that never themselves allocate (like [Texp_let _] expressions) are
      skipped
    - Nodes that might allocate and were assigned an allocation mode during
      typechecking return that mode in [Alloc_mode _]
    - Nodes that have a [Mode.Alloc.t option] field that is [None] because the
      corresponding expression doesn't allocate for a known reason (for example,
      a [Texp_record] corresponding to an unboxed record) return that reason in
      [No_alloc _]
    - Nodes that have a [Mode.Alloc.t option] field that is [None] without a
      known reason (usually because they didn't typecheck) return
      [Unexpected_no_alloc]
*)

val log_section : string

type stack_or_heap =
  | Alloc_mode of Mode.Alloc.r
  | No_alloc of { reason : string }
  | Unexpected_no_alloc

type stack_or_heap_enclosings = (Location.t * stack_or_heap) list

val from_nodes :
  lsp_compat:bool ->
  pos:Lexing.position ->
  path:(Env.t * Browse_raw.node * Query_protocol.is_tail_position) list ->
  stack_or_heap_enclosings
