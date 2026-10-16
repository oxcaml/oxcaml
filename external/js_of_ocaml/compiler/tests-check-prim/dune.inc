(rule
 (targets main.5.4.output)
 (deps main.bc)
 (mode
  (promote (until-clean)))
 (enabled_if (and (>= %{ocaml_version} 5.4)(< %{ocaml_version} 5.5)(not %{ocaml-config:ox})))
 (action
  (with-stdout-to
   %{targets}
   (run
    %{bin:js_of_ocaml}
    check-runtime
    +dynlink.js
    +toplevel.js
    main.bc))))

(rule
 (targets unix-Win32.5.4.output)
 (deps unix.bc)
 (mode
  (promote (until-clean)))
 (enabled_if (and (>= %{ocaml_version} 5.4)(< %{ocaml_version} 5.5)(= %{os_type} Win32)(not %{ocaml-config:ox})))
 (action
  (with-stdout-to
   %{targets}
   (run
    %{bin:js_of_ocaml}
    check-runtime
    +dynlink.js
    +toplevel.js
    unix.bc))))

(rule
 (targets unix-Unix.5.4.output)
 (deps unix.bc)
 (mode
  (promote (until-clean)))
 (enabled_if (and (>= %{ocaml_version} 5.4)(< %{ocaml_version} 5.5)(= %{os_type} Unix)(not %{ocaml-config:ox})))
 (action
  (with-stdout-to
   %{targets}
   (run
    %{bin:js_of_ocaml}
    check-runtime
    +dynlink.js
    +toplevel.js
    unix.bc))))

(rule
 (targets main.5.2+ox.output)
 (deps main.bc)
 (mode
  (promote (until-clean)))
 (enabled_if (and (>= %{ocaml_version} 5.2)(< %{ocaml_version} 5.3)%{ocaml-config:ox}))
 (action
  (with-stdout-to
   %{targets}
   (run
    %{bin:js_of_ocaml}
    check-runtime
    +dynlink.js
    +toplevel.js
    main.bc))))

(rule
 (targets unix-Win32.5.2+ox.output)
 (deps unix.bc)
 (mode
  (promote (until-clean)))
 (enabled_if (and (>= %{ocaml_version} 5.2)(< %{ocaml_version} 5.3)(= %{os_type} Win32)%{ocaml-config:ox}))
 (action
  (with-stdout-to
   %{targets}
   (run
    %{bin:js_of_ocaml}
    check-runtime
    +dynlink.js
    +toplevel.js
    unix.bc))))

(rule
 (targets unix-Unix.5.2+ox.output)
 (deps unix.bc)
 (mode
  (promote (until-clean)))
 (enabled_if (and (>= %{ocaml_version} 5.2)(< %{ocaml_version} 5.3)(= %{os_type} Unix)%{ocaml-config:ox}))
 (action
  (with-stdout-to
   %{targets}
   (run
    %{bin:js_of_ocaml}
    check-runtime
    +dynlink.js
    +toplevel.js
    unix.bc))))

