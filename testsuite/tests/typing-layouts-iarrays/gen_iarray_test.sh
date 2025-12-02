#!/bin/bash
# Generates an iarray test from a mutable array test.
#
# Usage: ./gen_iarray_test.sh <mutable_test.ml> <output_iarray_test.ml>
#
# Example:
#   ./gen_iarray_test.sh ../typing-layouts-arrays/test_ignorable_product_array_1.ml \
#                        test_ignorable_product_iarray_1.ml

set -e

if [ $# -ne 2 ]; then
    echo "Usage: $0 <mutable_test.ml> <output_iarray_test.ml>" >&2
    exit 1
fi

INPUT="$1"
OUTPUT="$2"

if [ ! -f "$INPUT" ]; then
    echo "Error: Input file '$INPUT' not found" >&2
    exit 1
fi

# Extract the customizable section between the two marker comments.
# The customizable section starts after "test directory. *)" and ends before
# "(* Below here is copy pasted".
# We use awk to avoid issues with multi-line comments containing similar text.
CUSTOMIZABLE=$(awk '
    /test directory\. \*\)/ && !started { started=1; next }
    /^\(\* Below here is copy pasted/ { exit }
    started { print }
' "$INPUT")

# Write the header
cat > "$OUTPUT" << 'HEADER_EOF'
(* TEST
 include stdlib_stable;
 include stdlib_upstream_compatible;
 readonly_files =
   "gen_u_iarray.ml test_gen_u_iarray.ml gen_product_iarray_helpers.ml";
 modules = "${readonly_files}";
 flambda2;
 {
   bytecode;
 }{
   native;
 }{
   flags = "-O3";
   native;
 }{
   flags = "-Oclassic";
   native;
 }
*)

open Gen_product_iarray_helpers
open Stdlib_stable
open Stdlib_upstream_compatible

(* This test is auto-generated from the corresponding mutable array test in
   typing-layouts-arrays/ using gen_iarray_test.sh. Do not edit directly.
   See README.md in this test directory. *)
HEADER_EOF

# Append the customizable section
echo "$CUSTOMIZABLE" >> "$OUTPUT"

# Append the boilerplate
cat >> "$OUTPUT" << 'BOILERPLATE_EOF'
(* Below here is copy pasted due to the absence of layout polymorphism. Don't
   change it.  See README.md in this test directory. *)
module Element_ops = (val Gen_product_iarray_helpers.make_element_ops elem)

module UTuple_array0 :
  Gen_u_iarray.S0 with type element_t = unboxed_t
                  and type ('a : any) array_t = 'a iarray
                  and type mutable_t = unboxed_t array = struct
  type element_t = unboxed_t

  type ('a : any) array_t = 'a iarray
  type mutable_t = unboxed_t array

  type element_arg = unit -> element_t
  type t = element_t iarray
  let max_length = Sys.max_array_length
  external length : element_t iarray -> int = "%array_length"
  external get: element_t iarray -> int -> element_t = "%array_safe_get"
  let get t i = let a = get t i in fun () -> a
  external unsafe_get: element_t iarray -> int -> element_t = "%array_unsafe_get"
  let unsafe_get t i = let a = unsafe_get t i in fun () -> a

  external makearray_dynamic : int -> element_t -> element_t array =
    "%makearray_dynamic"

  let unsafe_create_mutable : int -> mutable_t =
    (* We don't actually have an uninitialized creation function for these, yet,
       so we just use [makearray_dynamic] (which is what we want to test anyway)
       with the zero element. *)
    fun i ->
      if i < 0 || i > max_length then invalid_arg "unsafe_create_mutable";
      makearray_dynamic i (zero ())

  external unsafe_set_mutable: element_t array -> int -> element_t -> unit =
    "%array_unsafe_set"

  let unsafe_set_mutable t i e = unsafe_set_mutable t i (e ())

  external unsafe_get_mutable: element_t array -> int -> element_t =
    "%array_unsafe_get"

  let unsafe_get_mutable t i = let a = unsafe_get_mutable t i in fun () -> a

  let blit_to_mutable src src_off dst dst_off len =
    for i = 0 to len - 1 do
      unsafe_set_mutable dst (dst_off + i) (unsafe_get src (src_off + i))
    done

  external freeze : element_t array -> element_t iarray = "%array_to_iarray"

  let empty () : unboxed_t iarray =
    let m = unsafe_create_mutable 0 in
    freeze m

  let to_boxed = to_boxed

  let compare_element x y =
    Element_ops.compare (to_boxed (x ())) (to_boxed (y ()))
end

module UTuple_array = Gen_u_iarray.Make (UTuple_array0)

module UTuple_array_boxed = Test_gen_u_iarray.Make_boxed (struct
    module M = UTuple_array
    module I = Element_ops
    module E = struct
      let to_boxed x = to_boxed (x ())
      let of_boxed x () = of_boxed x
    end
  end)
module _ = Test_gen_u_iarray.Test (UTuple_array_boxed)
BOILERPLATE_EOF

echo "Generated $OUTPUT from $INPUT"
