type ('a : any) t : value_or_null & bits64 = 'a addr

external to_parts
  : ('a : any).
  ('a t[@local_opt]) @ immutable
  -> (#(Obj.t * (Obj.t, 'a) idx_mut)[@local_opt])
  = "%obj_magic"
external of_parts
  : ('c : value_or_null) ('a : any).
  (#('c * ('c, 'a) idx_mut)[@local_opt]) @ immutable
  -> ('a t[@local_opt])
  = "%obj_magic"

let[@zero_alloc] of_idx : ('a : value) ('b : any).
  'a -> ('a, 'b) idx_mut -> 'b t =
 fun obj idx -> of_parts #(obj, idx)
let[@zero_alloc] of_idx__local : ('a : value) ('b : any).
  'a @ local -> ('a, 'b) idx_mut -> 'b t @ local =
 fun obj idx -> exclave_ of_parts #(obj, idx)
let[@zero_alloc] of_idx__read : ('a : value) ('b : any).
  'a @ read -> ('a, 'b) idx_mut -> 'b t @ read =
 fun obj idx -> of_parts #(obj, idx)
let[@zero_alloc] of_idx__read__local : ('a : value) ('b : any).
  'a @ local read -> ('a, 'b) idx_mut -> 'b t @ local read =
 fun obj idx -> exclave_ of_parts #(obj, idx)
let[@zero_alloc] of_idx__write : ('a : value) ('b : any).
  'a @ write -> ('a, 'b) idx_mut -> 'b t @ write =
 fun obj idx -> of_parts #(obj, idx)
let[@zero_alloc] of_idx__write__local : ('a : value) ('b : any).
  'a @ local write -> ('a, 'b) idx_mut -> 'b t @ local write =
 fun obj idx -> exclave_ of_parts #(obj, idx)

external of_imm : ('a : any). 'a Addr_imm.t @ read -> 'a t @ read =
  "%obj_magic"
external of_imm__local
  : ('a : any). 'a Addr_imm.t @ local read -> 'a t @ local read =
  "%obj_magic"

let deepen : ('a : any) ('b : any).
  'a t @ immutable
  -> f:(('c : value_or_null). ('c, 'a) idx_mut -> ('c, 'b) idx_mut)
     @ local once
  -> 'b t =
 fun t ~f ->
  let #(base, idx) = to_parts t in
  of_parts #(base, f idx)
let deepen__local : ('a : any) ('b : any).
  'a t @ local immutable
  -> f:(('c : value_or_null). ('c, 'a) idx_mut -> ('c, 'b) idx_mut)
     @ local once
  -> 'b t @ local =
 fun t ~f -> exclave_
  let #(base, idx) = to_parts t in
  of_parts #(base, f idx)
let deepen__read = deepen
let deepen__read__local = deepen__local
let deepen__write = deepen
let deepen__write__local = deepen__local

external get
  : ('a : any).
  ('a t[@local_opt]) -> ('a[@local_opt])
  = "%unsafe_get_ptr"
[@@layout_poly]
external get__read
  : ('a : any).
  ('a t[@local_opt]) @ read -> ('a[@local_opt]) @ read
  = "%unsafe_get_ptr"
[@@layout_poly]

external set
  : ('a : any mod external64).
  ('a t[@local_opt]) @ write -> 'a -> unit
  = "%unsafe_set_ptr"
[@@layout_poly]
external modify
  : ('a : any).
  ('a t[@local_opt]) @ write -> 'a -> unit
  = "%unsafe_set_ptr"
[@@layout_poly]
