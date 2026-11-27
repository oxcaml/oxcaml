set -eu

group=$1
stdlib=$2
shift 2

reject() {
  expected=$1
  shift
  cat > rejected.ml
  if "$@" -nostdlib -I "$stdlib" -stop-after typing -c rejected.ml \
      > diagnostic 2>&1; then
    echo "Expected rejection: $expected" >&2
    cat rejected.ml >&2
    exit 1
  fi
  if ! grep -F -- "$expected" diagnostic > /dev/null; then
    cat diagnostic >&2
    exit 1
  fi
}

case "$group" in
interfaces)
  reject 'The primitive %unsafe_set_ext_ptr does not support inline sums.' "$@" <<'EOF'
external[@layout_poly] store : ('a : any). int64_u -> 'a -> unit
  = "%unsafe_set_ext_ptr"
let write p (value : Sum_storage.t) = store p value
EOF
  reject 'The primitive %unsafe_set_ext_ptr does not support inline sums.' \
    "$@" -extension layout_poly_alpha <<'EOF'
external[@layout_poly] store : ('a : any). int64_u -> 'a -> unit
  = "%unsafe_set_ext_ptr"
let poly_ write p value = store p value
EOF
  reject 'cannot create an array whose elements' \
    "$@" -extension layout_poly_alpha <<'EOF'
external[@layout_poly] make : ('a : any separable). int -> 'a array
  = "%makearray_dynamic_uninit"
let poly_ create (witness : 'a) n : 'a array = make n
EOF
  reject 'The primitive %peek does not support inline sums.' "$@" <<'EOF'
external[@layout_poly] peek : ('a : any). nativeint_u -> 'a = "%peek"
let read p : Sum_storage.t = peek p
EOF
  reject 'The primitive %poke does not support inline sums.' "$@" <<'EOF'
external[@layout_poly] poke : ('a : any). nativeint_u -> 'a -> unit = "%poke"
let write p (value : Sum_storage.pair) = poke p value
EOF
  cat > generic_wrapper.ml <<'EOF'
open Write_access
let poly_ replace_any (dst @ write uncontended) value = weak_set dst 0 value
EOF
  "$@" -nostdlib -I "$stdlib" -extension layout_poly_alpha \
    -stop-after typing -c generic_wrapper.ml
  for primitive in '%arrayblit' '%arrayblit_src_immut'; do
    if [ "$primitive" = '%arrayblit' ]; then src=array; else src=iarray; fi
    reject "destination of $primitive must require uncontended access" "$@" <<EOF
open Sum_storage
external copy : t $src -> int -> t array @ write corrupted -> int -> int -> unit
  = "$primitive"
EOF
  done
  reject 'destination of %arrayblit must require uncontended access' "$@" <<'EOF'
open Sum_storage
external copy : pair array -> int -> pair array @ write contended ->
  int -> int -> unit = "%arrayblit"
EOF
  for element in 'Sum_storage.t' 'Sum_storage.pair'; do
    reject 'cannot create an array whose elements' "$@" <<EOF
external make : int -> $element array = "%makearray_dynamic_uninit"
EOF
  done
  reject 'cannot create an array whose elements' "$@" <<'EOF'
type storage = Wrap of Sum_storage.t array [@@unboxed]
external make : int -> storage = "%makearray_dynamic_uninit"
EOF
  reject 'destination of %arrayblit must require uncontended access' "$@" <<'EOF'
type storage = private Sum_storage.t array
external copy : storage -> int -> storage @ write corrupted -> int -> int -> unit
  = "%arrayblit"
EOF
  reject 'corrupted but is expected to be uncontended' "$@" <<'EOF'
open Sum_storage
let copy_one src (dst : t array @ write corrupted) = copy src dst
EOF
  ;;
replacements)
  cat > client.ml <<'EOF'
open Replacements
let run (dst : target array @ write uncontended) value = replace_weak dst value
EOF
  "$@" -nostdlib -I "$stdlib" -c client.ml
  for mode in corrupted contended; do
    reject "This value is $mode" "$@" <<EOF
open Replacements
let set (dst : cell @ write $mode) value = dst.data <- value
EOF
    if [ "$mode" = corrupted ]; then
      expected='expression is corrupted but is expected to be uncontended'
    else
      expected='expected to be corrupted or uncontended'
    fi
    reject "$expected" "$@" <<EOF
open Replacements
let set (dst : target array @ write $mode) value = weak_set dst 0 value
EOF
  done
  reject 'uncontended' "$@" <<'EOF'
open Replacements
let set (dst : pair_cell @ write corrupted) value = dst.data_pair <- value
EOF
  reject 'uncontended' "$@" <<'EOF'
open Replacements
let set (dst : target array @ write corrupted) value = set dst 0 value
EOF
  reject 'uncontended' "$@" <<'EOF'
open Replacements
let set (dst : target array @ write corrupted) = weak_set dst
EOF
  reject 'destination of %array_safe_set must require uncontended access' "$@" <<'EOF'
open Replacements
let first_class : target array @ write corrupted -> int -> target -> unit =
  weak_set
EOF
  reject 'destination of %array_safe_set must require uncontended access' "$@" <<'EOF'
open Replacements
let accept (fn : target array @ write corrupted -> int -> target -> unit) = fn
let higher_order = accept weak_set
EOF
  reject 'corrupted but is expected to be uncontended' "$@" <<'EOF'
open Replacements
let set (dst : target ref @ corrupted) value = dst := value
EOF
  reject 'Mutable variable cannot be used inside a function' "$@" <<'EOF'
open Replacements
let capture (v : target) = let mutable slot = v in fun next -> slot <- next
EOF
  reject 'read but is expected to be write or read_write' "$@" <<'EOF'
open Replacements
let set (dst : target array @ read uncontended) value = weak_set dst 0 value
EOF
  reject 'corrupted but is expected to be uncontended' "$@" <<'EOF'
open Replacements
let run (dst : target array @ write corrupted) value = replace_weak dst value
EOF
  reject 'uncontended' "$@" <<'EOF'
open Replacements
let set (dst : target addr @ write corrupted) value = store dst value
EOF
  reject 'uncontended' "$@" <<'EOF'
open Replacements
let set (dst : cell @ write corrupted) value = set_idx dst (.data) value
EOF
  reject 'cannot create an array whose elements' "$@" <<'EOF'
open Replacements
external[@layout_poly] uninit : ('a : any separable). int -> 'a array
  = "%makearray_dynamic_uninit"
let make count : target array = uninit count
EOF
  ;;
*) exit 1 ;;
esac
