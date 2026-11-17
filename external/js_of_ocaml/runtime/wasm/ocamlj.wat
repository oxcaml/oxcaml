;; Wasm_of_ocaml runtime support
;; http://www.ocsigen.org/js_of_ocaml/
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, with linking exception;
;; either version 2.1 of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; Runtime support for code produced by the OxCaml JSIR backend, which
;; translates Flambda 2 terms to the js_of_ocaml IR instead of going through
;; bytecode. See also runtime/js/ocamlj.js.

(module
   (import "fail" "caml_failwith" (func $caml_failwith (param (ref eq))))
   (import "obj" "null" (global $null (ref eq)))
   (import "string" "caml_string_concat"
      (func $caml_string_concat
         (param (ref eq)) (param (ref eq)) (result (ref eq))))

   ;; [@string] expands to [array.new_fixed $bytes ...]
   (type $bytes (array (mut i8)))
   (type $float (struct (field f64)))

   (@string $invalid_switch_arm
      "caml_invalid_switch_arm: encountered invalid switch arm, this is a bug in Flambda2 or the Flambda2 -> JSIR pass")

   (func (export "caml_invalid_switch_arm") (result (ref eq))
      (call $caml_failwith (global.get $invalid_switch_arm))
      (ref.i31 (i32.const 0)))

   (@string $invalid_primitive
      "caml_invalid_primitive: encountered an invalid primitive, this is a bug in Flambda2 or the Flambda2 -> JSIR pass")

   (func (export "caml_invalid_primitive") (result (ref eq))
      (call $caml_failwith (global.get $invalid_primitive))
      (ref.i31 (i32.const 0)))

   (@string $invalid_expr
      "caml_invalid_expr: reached an Invalid Flambda2 expression: ")

   ;; [msg] is an OCaml string.
   (func (export "caml_invalid_expr") (param $msg (ref eq)) (result (ref eq))
      (call $caml_failwith
         (call $caml_string_concat
            (global.get $invalid_expr) (local.get $msg)))
      (ref.i31 (i32.const 0)))

   (@string $unimplemented_ext_ptr
      "External ptrs are unimplemented in WebAssembly")

   ;; caml_check_idx_base : value -> value
   ;; Returns the base of a block index access unchanged, unless it is [Null],
   ;; in which case the index is a raw address (an external pointer) that
   ;; cannot be dereferenced here, and [Failure] is raised.
   (func (export "caml_check_idx_base") (param $base (ref eq)) (result (ref eq))
      (if (ref.eq (local.get $base) (global.get $null))
         (then (call $caml_failwith (global.get $unimplemented_ext_ptr))))
      (local.get $base))

   ;; caml_is_boxed_float : value -> bool
   (func (export "caml_is_boxed_float") (param $v (ref eq)) (result (ref eq))
      (ref.i31 (ref.test (ref $float) (local.get $v))))
)
