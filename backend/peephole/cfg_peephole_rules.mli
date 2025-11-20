[@@@ocaml.warning "+a-29-40-41-42"]

open! Cfg_peephole_utils

val apply :
  Cfg.basic Cfg.instruction DLL.cell ->
  Cfg.basic Cfg.instruction DLL.cell option
