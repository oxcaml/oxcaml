type ('a : any) t : value_or_null & bits64 = 'a addr

val of_idx
  : ('a : value) ('b : any).
  'a -> ('a, 'b) idx_mut -> 'b t
[@@zero_alloc]
val of_idx__local
  : ('a : value) ('b : any).
  'a @ local -> ('a, 'b) idx_mut -> 'b t @ local
[@@zero_alloc]
val of_idx__read
  : ('a : value) ('b : any).
  'a @ read -> ('a, 'b) idx_mut -> 'b t @ read
[@@zero_alloc]
val of_idx__read__local
  : ('a : value) ('b : any).
  'a @ local read -> ('a, 'b) idx_mut -> 'b t @ local read
[@@zero_alloc]
val of_idx__write
  : ('a : value) ('b : any).
  'a @ write -> ('a, 'b) idx_mut -> 'b t @ write
[@@zero_alloc]
val of_idx__write__local
  : ('a : value) ('b : any).
  'a @ local write -> ('a, 'b) idx_mut -> 'b t @ local write
[@@zero_alloc]
val of_imm : ('a : any). 'a Addr_imm.t @ read -> 'a t @ read
[@@zero_alloc]
val of_imm__local
  : ('a : any). 'a Addr_imm.t @ local read -> 'a t @ local read
[@@zero_alloc]

val deepen
  : ('a : any) ('b : any).
  'a t
  -> f:(('c : value_or_null). ('c, 'a) idx_mut -> ('c, 'b) idx_mut)
     @ local once
  -> 'b t
val deepen__local
  : ('a : any) ('b : any).
  'a t @ local
  -> f:(('c : value_or_null). ('c, 'a) idx_mut -> ('c, 'b) idx_mut)
     @ local once
  -> 'b t @ local
val deepen__read
  : ('a : any) ('b : any).
  'a t @ read
  -> f:(('c : value_or_null). ('c, 'a) idx_mut -> ('c, 'b) idx_mut)
     @ local once
  -> 'b t @ read
val deepen__read__local
  : ('a : any) ('b : any).
  'a t @ local read
  -> f:(('c : value_or_null). ('c, 'a) idx_mut -> ('c, 'b) idx_mut)
     @ local once
  -> 'b t @ local read
val deepen__write
  : ('a : any) ('b : any).
  'a t @ write
  -> f:(('c : value_or_null). ('c, 'a) idx_mut -> ('c, 'b) idx_mut)
     @ local once
  -> 'b t @ write
val deepen__write__local
  : ('a : any) ('b : any).
  'a t @ local write
  -> f:(('c : value_or_null). ('c, 'a) idx_mut -> ('c, 'b) idx_mut)
     @ local once
  -> 'b t @ local write

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
