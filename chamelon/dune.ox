(executable
 (name chamelon)
 (public_name chamelon)
 (modes native)
 ; We don't use [unix] in [libraries] as that causes ocaml/otherlibs/unix/
 ; to be built against the system compiler, which won't work.
 (ocamlc_flags (:standard -I +unix unix.cma))
 (ocamlopt_flags (:standard -I +unix unix.cmxa))
 (libraries ocamlcommon
   (select compat.ml from ( -> compat.ox.ml))
 )
 (package ocaml)
 (enabled_if (= %{context_name} "default"))
)

(executable
 (name chamelon)
 (public_name chamelon)
 (modes native)
 (libraries ocamlcommon unix
   (select compat.ml from ( -> compat.ox.ml))
 )
 (package ocaml)
 (enabled_if (= %{context_name} "main"))
)

(env
  (dev
    (flags (:standard -no-principal -warn-error -A -w -70)))
  (_
    (flags (:standard -no-principal -w -70))))

(include_subdirs unqualified)
