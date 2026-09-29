type ('a : any) t : value_or_null & bits64 = 'a addr_imm

val of_idx
  : ('a : value) ('b : any).
  'a -> ('a, 'b) idx_imm -> 'b t
[@@zero_alloc]
val of_idx__local
  : ('a : value) ('b : any).
  'a @ local -> ('a, 'b) idx_imm -> 'b t @ local
[@@zero_alloc]
val of_idx__read
  : ('a : value) ('b : any).
  'a @ read -> ('a, 'b) idx_imm -> 'b t @ read
[@@zero_alloc]
val of_idx__read__local
  : ('a : value) ('b : any).
  'a @ local read -> ('a, 'b) idx_imm -> 'b t @ local read
[@@zero_alloc]
val of_idx__write
  : ('a : value) ('b : any).
  'a @ write -> ('a, 'b) idx_imm -> 'b t @ write
[@@zero_alloc]
val of_idx__write__local
  : ('a : value) ('b : any).
  'a @ local write -> ('a, 'b) idx_imm -> 'b t @ local write
[@@zero_alloc]
val of_idx__immutable
  : ('a : value) ('b : any).
  'a @ immutable -> ('a, 'b) idx_imm -> 'b t @ immutable
[@@zero_alloc]
val of_idx__immutable__local
  : ('a : value) ('b : any).
  'a @ local immutable -> ('a, 'b) idx_imm -> 'b t @ local immutable
[@@zero_alloc]

val deepen
  : ('a : any) ('b : any).
  'a t
  -> f:(('c : value_or_null). ('c, 'a) idx_imm -> ('c, 'b) idx_imm)
     @ local once
  -> 'b t
val deepen__local
  : ('a : any) ('b : any).
  'a t @ local
  -> f:(('c : value_or_null). ('c, 'a) idx_imm -> ('c, 'b) idx_imm)
     @ local once
  -> 'b t @ local
val deepen__read
  : ('a : any) ('b : any).
  'a t @ read
  -> f:(('c : value_or_null). ('c, 'a) idx_imm -> ('c, 'b) idx_imm)
     @ local once
  -> 'b t @ read
val deepen__read__local
  : ('a : any) ('b : any).
  'a t @ local read
  -> f:(('c : value_or_null). ('c, 'a) idx_imm -> ('c, 'b) idx_imm)
     @ local once
  -> 'b t @ local read
val deepen__write
  : ('a : any) ('b : any).
  'a t @ write
  -> f:(('c : value_or_null). ('c, 'a) idx_imm -> ('c, 'b) idx_imm)
     @ local once
  -> 'b t @ write
val deepen__write__local
  : ('a : any) ('b : any).
  'a t @ local write
  -> f:(('c : value_or_null). ('c, 'a) idx_imm -> ('c, 'b) idx_imm)
     @ local once
  -> 'b t @ local write
val deepen__immutable
  : ('a : any) ('b : any).
  'a t @ immutable
  -> f:(('c : value_or_null). ('c, 'a) idx_imm -> ('c, 'b) idx_imm)
     @ local once
  -> 'b t @ immutable
val deepen__immutable__local
  : ('a : any) ('b : any).
  'a t @ local immutable
  -> f:(('c : value_or_null). ('c, 'a) idx_imm -> ('c, 'b) idx_imm)
     @ local once
  -> 'b t @ local immutable

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
