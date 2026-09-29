type ('a : any) t : value_or_null & bits64 = 'a addr_imm

external to_parts
  : ('a : any).
  ('a t[@local_opt]) @ immutable
  -> (#(Obj.t * (Obj.t, 'a) idx_imm)[@local_opt])
  = "%obj_magic"
external of_parts
  : ('c : value_or_null) ('a : any).
  (#('c * ('c, 'a) idx_imm)[@local_opt]) @ immutable
  -> ('a t[@local_opt])
  = "%obj_magic"

let[@zero_alloc] of_idx : ('a : value) ('b : any).
  'a -> ('a, 'b) idx_imm -> 'b t =
 fun obj idx -> of_parts #(obj, idx)
let[@zero_alloc] of_idx__local : ('a : value) ('b : any).
  'a @ local -> ('a, 'b) idx_imm -> 'b t @ local =
 fun obj idx -> exclave_ of_parts #(obj, idx)
let[@zero_alloc] of_idx__read : ('a : value) ('b : any).
  'a @ read -> ('a, 'b) idx_imm -> 'b t @ read =
 fun obj idx -> of_parts #(obj, idx)
let[@zero_alloc] of_idx__read__local : ('a : value) ('b : any).
  'a @ local read -> ('a, 'b) idx_imm -> 'b t @ local read =
 fun obj idx -> exclave_ of_parts #(obj, idx)
let[@zero_alloc] of_idx__write : ('a : value) ('b : any).
  'a @ write -> ('a, 'b) idx_imm -> 'b t @ write =
 fun obj idx -> of_parts #(obj, idx)
let[@zero_alloc] of_idx__write__local : ('a : value) ('b : any).
  'a @ local write -> ('a, 'b) idx_imm -> 'b t @ local write =
 fun obj idx -> exclave_ of_parts #(obj, idx)
let[@zero_alloc] of_idx__immutable : ('a : value) ('b : any).
  'a @ immutable -> ('a, 'b) idx_imm -> 'b t @ immutable =
 fun obj idx -> of_parts #(obj, idx)
let[@zero_alloc] of_idx__immutable__local : ('a : value) ('b : any).
  'a @ local immutable -> ('a, 'b) idx_imm -> 'b t @ local immutable =
 fun obj idx -> exclave_ of_parts #(obj, idx)

let deepen : ('a : any) ('b : any).
  'a t @ immutable
  -> f:(('c : value_or_null). ('c, 'a) idx_imm -> ('c, 'b) idx_imm)
     @ local once
  -> 'b t =
 fun t ~f ->
  let #(base, idx) = to_parts t in
  of_parts #(base, f idx)
let deepen__local : ('a : any) ('b : any).
  'a t @ local immutable
  -> f:(('c : value_or_null). ('c, 'a) idx_imm -> ('c, 'b) idx_imm)
     @ local once
  -> 'b t @ local =
 fun t ~f -> exclave_
  let #(base, idx) = to_parts t in
  of_parts #(base, f idx)
let deepen__read = deepen
let deepen__read__local = deepen__local
let deepen__write = deepen
let deepen__write__local = deepen__local
let deepen__immutable = deepen
let deepen__immutable__local = deepen__local

external get
  : ('a : any).
  ('a t[@local_opt]) -> ('a[@local_opt])
  = "%unsafe_get_ptr_imm"
[@@layout_poly]
external get__read
  : ('a : any).
  ('a t[@local_opt]) @ read -> ('a[@local_opt]) @ read
  = "%unsafe_get_ptr_imm"
[@@layout_poly]
external get__write
  : ('a : any).
  ('a t[@local_opt]) @ write -> ('a[@local_opt]) @ write
  = "%unsafe_get_ptr_imm"
[@@layout_poly]
external get__immutable
  : ('a : any).
  ('a t[@local_opt]) @ immutable -> ('a[@local_opt]) @ immutable
  = "%unsafe_get_ptr_imm"
[@@layout_poly]
