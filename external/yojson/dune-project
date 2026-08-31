(lang dune 2.7)
(name yojson)

(generate_opam_files true)
(source (github ocaml-community/yojson))
(license BSD-3-Clause)

(maintainers "paul-elliot@tarides.com" "nathan@tarides.com" "marek@tarides.com")
(authors "Martin Jambon")

(package
  (name yojson)
  (synopsis "Yojson is an optimized parsing and printing library for the JSON format")
  (description "Yojson is an optimized parsing and printing library for the JSON format.

ydump is a pretty-printing command-line program provided with the
yojson package.")
  (documentation "https://ocaml.org/p/yojson/latest")
  (depends
    (ocaml (>= 4.08))
    (alcotest (and :with-test (>= 0.8.5)))))

(package
  (name yojson-five)
  (synopsis "Yojson-five is a parsing and printing library for the JSON5 format")
  (description "Yojson-five is a parsing and printing library for the JSON5 format.
It supports parsing JSON5 to Yojson.Basic.t and Yojson.Safe.t types.")
  (documentation "https://ocaml.org/p/yojson-five/latest")
  (depends
    (ocaml (>= 4.08))
    (sedlex (>= 2.5))
    (yojson (= :version))
    (alcotest (and :with-test (>= 0.8.5)))))

(package
  (name yojson-bench)
  (synopsis "Run Yojson benchmarks")
  (description "Yojson benchmarks require `Core_bench` which is not a dependency of Yojson,
because it is not part of the regular installation/testing flow. This is solely
meant for developers that are worried about performance changes in Yojson.")
  (depends
    (ocaml (>= 4.14))
    (yojson (= :version))
    benchmark))
