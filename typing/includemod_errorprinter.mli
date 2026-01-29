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

<<<<<<< HEAD
val err_msgs: Includemod.explanation -> Format.formatter -> unit
||||||| parent of 1b09b92c85 (Merge pull request #13169 from Octachron/format_doc_for_error_messages)
val err_msgs: Includemod.explanation -> Format.formatter -> unit
val coercion_in_package_subtype:
  Env.t -> Types.module_type -> Typedtree.module_coercion -> Format.formatter ->
  unit
=======
val err_msgs: Includemod.explanation Format_doc.printer
val coercion_in_package_subtype:
  Env.t -> Types.module_type -> Typedtree.module_coercion -> Format_doc.doc
>>>>>>> 1b09b92c85 (Merge pull request #13169 from Octachron/format_doc_for_error_messages)
val register: unit -> unit
