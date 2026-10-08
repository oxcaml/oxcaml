In 5.2 the format of ocaml.ppx.context load_path changed.
To ensure compat, we defined migration for ocaml.ppx.context attributes

We write such an attribute to an .ml file. The compiler will add its own
and it should be consumed by the driver but our handwritten attribute will
be migrated as well and should remain in the AST.
  $ cat > test.ml << EOF
  > let x = 1
  > [@@@ocaml.ppx.context
  >   {
  >     tool_name = "ocaml";
  >     include_dirs = ["foo"];
  >     hidden_include_dirs = [];
  >     load_path = (["foo"; "bar"], ["baz"]);
  >     open_modules = [];
  >     for_package = None;
  >     debug = true;
  >     use_threads = false;
  >     use_vmthreads = false;
  >     recursive_types = false;
  >     principal = false;
  >     transparent_modules = false;
  >     unboxed_types = false;
  >     unsafe_string = false;
  >     cookies = []
  >   }]
  > EOF

We then run a custom driver that will read our ast, migrate it back to 5.01,
pretty print the ocaml.ppx.context, convert it back to the latest version and
pretty print it again. This last, round-tripped version should be identical to
the one above.

  $ ./driver.exe --impl test.ml -o ignore.ml
  ocaml.ppx.context before 5.02:
  [@@@ocaml.ppx.context
    {
      tool_name: "ocaml";
      include_dirs: ["foo"];
      hidden_include_dirs: [];
      load_path: ["foo"; "bar"; "baz"];
      open_modules: [];
      for_package: None;
      debug: true;
      use_threads: false;
      use_vmthreads: false;
      recursive_types: false;
      principal: false;
      transparent_modules: false;
      unboxed_types: false;
      unsafe_string: false;
      cookies: [];
    }
  ]
  ocaml.ppx.context round tripped:
  [@@@ocaml.ppx.context
    {
      tool_name = "ocaml";
      include_dirs = ["foo"];
      hidden_include_dirs = [];
      load_path = (["foo"; "bar"], ["baz"]);
      open_modules = [];
      for_package = None;
      debug = true;
      use_threads = false;
      use_vmthreads = false;
      recursive_types = false;
      principal = false;
      transparent_modules = false;
      unboxed_types = false;
      unsafe_string = false;
      cookies = []
    }]
