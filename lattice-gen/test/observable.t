  $ mkdir pages
  $ cp observable-fixtures/basic.html pages/basic.html
  $ cp observable-fixtures/filtered.html pages/filtered.html
  $ ../bin/lattice_gen.exe observable --update pages
  $ sed -n '1,8p' pages/basic.html
  <html>
    <head><title>Basic Observable</title><link rel="stylesheet" href="lattice-observable.css" data-lattice-observable="css">
  <script src="lattice-observable.js" data-lattice-observable="js"></script>
  </head>
    <body>
      <lat root-module="Demo">
  Locality = [
    Global < Local
  $ python3 - <<'PY'
  > text = open("pages/basic.html").read()
  > start = text.index("<out>")
  > stop = text.index("</out>", start) + len("</out>")
  > print("\n".join(text[start:stop].splitlines()[:8]))
  > PY
  <out>
  <div class="lattice-observable lat-observable" data-default-tab="ml">
  <div class="lattice-observable__header"><span class="lattice-observable__title">Demo</span><span class="lattice-observable__meta">generated lattice library</span></div>
  <div class="lattice-observable__tabs"><button class="lattice-observable__tab is-active" data-lattice-tab="ml">Demo.ml</button><button class="lattice-observable__tab" data-lattice-tab="mli">Demo.mli</button><button class="lattice-observable__tab" data-lattice-tab="test">Demo_test.ml</button></div>
  <pre class="lattice-observable__panel is-active" data-lattice-panel="ml">module Make_const_op (Base : sig
    type t
    val bottom : t
    val top : t
  $ python3 - <<'PY'
  > text = open("pages/basic.html").read()
  > start = text.index("<ocaml-out>")
  > stop = text.index("</ocaml-out>", start) + len("</ocaml-out>")
  > print(text[start:stop])
  > PY
  <ocaml-out>
  <div class="lattice-observable lattice-observable--ok">
  <div class="lattice-observable__header"><span class="lattice-observable__title">OCaml stdout</span></div>
  <pre class="lattice-observable__panel is-active">Local</pre>
  </div>
  </ocaml-out>
  $ python3 - <<'PY'
  > text = open("pages/filtered.html").read()
  > start = text.index("NoSolver.ml")
  > stop = text.index("</out>", start)
  > assert "module Solver" not in text[start:stop]
  > print("filtered-ok")
  > PY
  filtered-ok
  $ ../bin/lattice_gen.exe observable --check pages
