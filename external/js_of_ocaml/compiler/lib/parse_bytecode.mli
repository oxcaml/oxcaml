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

module Debug : sig
  type summary

  val is_empty : summary -> bool

  val default_summary : unit -> summary

  val paths : summary -> units:StringSet.t -> StringSet.t
end

type one =
  { code : Code.program
  ; cmis : StringSet.t
  ; debug : Debug.summary
  }

type compilation_unit =
  { name : string
  ; info : Unit_info.t
  ; contents : one
  }

val primitives : one -> string list

val load :
     filename:string
  -> include_dirs:string list (** unused *)
  -> include_cmis:bool (** unused *)
  -> debug:bool (** unused *)
  -> log_times:bool
  -> [ `Cmj of compilation_unit | `Cmja of compilation_unit list ]

val predefined_exceptions : unit -> Code.program * Unit_info.t
