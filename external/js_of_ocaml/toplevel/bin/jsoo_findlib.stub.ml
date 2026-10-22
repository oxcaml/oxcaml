let unsupported pkg =
  failwith
    (Printf.sprintf
       "findlib package %s: this tool was built without findlib; pass .cmi or .cma files \
        instead"
       pkg)

let package_directory pkg = unsupported pkg

let package_property _ pkg _ = unsupported pkg
