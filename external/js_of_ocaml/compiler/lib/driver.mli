(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Stdlib

type optimized_result =
  { program : Code.program
  ; variable_uses : Deadcode.variable_uses
  ; trampolined_calls : Effects.trampolined_calls
  ; in_cps : Effects.in_cps
  ; deadcode_sentinel : Code.Var.t
  ; shapes : Shape.t StringMap.t
  }

(** Optimize under the current target and effects configuration, stopping before
    field-sensitive dead-code elimination, exact-call/effects lowering, and
    [Generate_closure]. The result remains suitable for graph partitioning and
    subsequent ordinary compilation. This stage introduces no dead-field sentinel
    or backend lowering metadata. *)
val optimize_for_partition : profile:Profile.t -> Code.program -> Code.program

val optimize_for_wasm :
     shapes:bool
  -> profile:Profile.t
  -> Code.program
  -> optimized_result * (Global_flow.state * Global_flow.info)

type output =
  { sourcemaps : Source_map.info
  ; shapes : Shape.t StringMap.t
  ; primitives : StringSet.t
        (** The runtime accessor names the generated code uses, collected before
            JavaScript cleanup and runtime linking, for this compilation only. Aliases
            are resolved (unless the alias is called explicitly) and generated helpers
            are included; the closure of their runtime dependencies is not. *)
  }

val f :
  ?standalone:bool
  -> ?wrap_with_fun:[ `Iife | `Anonymous | `Named of string ]
  -> ?profile:Profile.t
  -> ?shapes:bool
  -> link:[ `All | `All_from of string list | `Needed | `No ]
  -> source_map:bool
  -> formatter:Pretty_print.t
  -> Code.program
  -> output

val f' 
  : ?standalone:bool
  -> ?wrap_with_fun:[ `Iife | `Anonymous | `Named of string ]
  -> ?profile:Profile.t
  -> link:[ `All | `All_from of string list | `Needed | `No ]
  -> Pretty_print.t
  -> Code.program
  -> unit

val from_string :
     prims:string array
  -> debug:Instruct.debug_event list array
  -> string
  -> Pretty_print.t
  -> unit

val link_and_pack :
     ?standalone:bool
  -> ?wrap_with_fun:[ `Iife | `Anonymous | `Named of string ]
  -> ?link:[ `All | `All_from of string list | `Needed | `No ]
  -> Javascript.statement_list
  -> Javascript.statement_list

val simplify_js : Javascript.statement_list -> Javascript.statement_list

val name_variables : Javascript.statement_list -> Javascript.statement_list

val configure : Pretty_print.t -> unit
