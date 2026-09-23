(* TEST
 readonly_files = "cells_dynlink_api.ml cells_dynlink_plugin.ml \
   cells_dynlink_plugin_nocmx.ml";
 include dynlink;
 libraries = "";
 flambda2;
 native-dynlink;
 setup-ocamlopt.byte-build-env;
 module = "cells_dynlink_api.ml";
 ocamlopt.byte;
 module = "cells_dynlink_plugin.ml";
 ocamlopt.byte;
 module = "cells_dynlink_host.ml";
 ocamlopt.byte;
 script = "mv cells_dynlink_api.cmx cells_dynlink_api.cmx.bak";
 script;
 flags = "-w -58";
 module = "cells_dynlink_plugin_nocmx.ml";
 ocamlopt.byte;
 script = "mv cells_dynlink_api.cmx.bak cells_dynlink_api.cmx";
 script;
 module = "";
 flags = "-shared";
 program = "cells_dynlink_plugin.cmxs";
 all_modules = "cells_dynlink_plugin.cmx";
 ocamlopt.byte;
 program = "cells_dynlink_plugin_nocmx.cmxs";
 all_modules = "cells_dynlink_plugin_nocmx.cmx";
 ocamlopt.byte;
 flags = "";
 libraries = "dynlink";
 program = "${test_build_directory}/cells_dynlink_host.exe";
 all_modules = "cells_dynlink_api.cmx cells_dynlink_host.cmx";
 ocamlopt.byte;
 run;
 check-program-output;
*)

(* natdynlink plugins reading a host unit's fields and whole block: the
   host exports its cells (and closures) like any other symbol. *)

let load name =
  try Dynlink.loadfile name with
  | Dynlink.Error e -> print_endline (Dynlink.error_message e)

let () =
  load "cells_dynlink_plugin.cmxs";
  Printf.printf "host after plugin: !r = %d\n" !Cells_dynlink_api.r;
  load "cells_dynlink_plugin_nocmx.cmxs";
  Printf.printf "host after nocmx: !r = %d\n" !Cells_dynlink_api.r;
  List.iter print_endline (List.rev !Cells_dynlink_api.report)
