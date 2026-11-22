(* TEST
 readonly_files = "check_replacements.sh";
 setup-ocamlc.byte-build-env;
 flags = "-c";
 module = "replacements.ml";
 ocamlc.byte;
 script = "sh check_replacements.sh replacements ${ocamlsrcdir}/stdlib ${ocamlrun} ${ocamlc_byte}";
 script;
*)

type target = Nothing | Item of int32_u [@@unboxed]
type cell = { mutable data : target; mutable sibling : int }
type pair_cell = { mutable data_pair : #(int * target) }
type view = { data_view : target }

external[@layout_poly] set : ('a : any separable).
  'a array @ write uncontended -> int -> 'a -> unit = "%array_safe_set"
external[@layout_poly] weak_set : ('a : any separable).
  'a array @ write corrupted -> int -> 'a -> unit = "%array_safe_set"
external[@layout_poly] store : ('a : any).
  'a addr @ write uncontended -> 'a -> unit = "%unsafe_set_ptr"
external[@layout_poly] set_idx : 'a ('b : any).
  'a @ write uncontended -> ('a, 'b) idx_mut -> 'b -> unit = "%set_idx"

let make data = { data; sibling = 0 }
let make_array n = [| Item n; Nothing |]
let update_local (v : target) next =
  let mutable slot = v in slot <- next; slot
let copy_with_data src data = { src with data }
let update (dst : cell @ write uncontended) data = dst.data <- data
let update_pair (dst : pair_cell @ write uncontended) v = dst.data_pair <- v
let update_sibling (dst : cell @ write corrupted) n = dst.sibling <- n
let read (src : view @ read shared) = src.data_view
let replace (dst : target array @ write uncontended) pos v = set dst pos v
let replace_weak (dst : target array @ write uncontended) v = weak_set dst 0 v
let replace_scalar (dst : int array @ write corrupted) v = weak_set dst 0 v
let partial (dst : target array @ write uncontended) = weak_set dst
let first_class : target array @ write uncontended -> int -> target -> unit =
  weak_set
let accept_setter (fn : target array @ write uncontended ->
                        int -> target -> unit) = fn
let higher_order = accept_setter weak_set
let update_ref (dst : target ref @ uncontended) v = dst := v
let via_address (dst : target addr @ write uncontended) v = store dst v
let via_index (dst : cell @ write uncontended) v = set_idx dst (.data) v
