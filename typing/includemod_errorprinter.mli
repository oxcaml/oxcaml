(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

val err_msgs: Includemod.explanation Format_doc.printer
<<<<<<< HEAD
||||||| 23e84b8c4d
val err_msgs: Includemod.explanation -> Format.formatter -> unit
=======
val coercion_in_package_subtype:
  Env.t -> Types.module_type -> Typedtree.module_coercion -> Format_doc.doc
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
val register: unit -> unit
