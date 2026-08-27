# js_of_ocaml build inputs. jsoo is compiled by the in-tree compiler:
# `make jsoo-build` uses _install and the dependency sources below (via the
# external/js_of_ocaml/vendor symlink); the `binaries` package does the same
# hermetically with a nix-built compiler. The jsoo_ppx driver is built with a
# stock compiler since no ppxlib is compatible with OxCaml yet.
{
  pkgs,
  dune,
  menhirSrc,
}:
let
  inherit (pkgs) lib;
  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_4;
  subtree = ./external/js_of_ocaml;
  version = lib.head (lib.splitString "\n" (lib.fileContents (subtree + "/VERSION")));

  targets = [
    "compiler/bin-js_of_ocaml/js_of_ocaml.exe"
    "compiler/bin-jsoo_minify/jsoo_minify.exe"
    "compiler/bin-wasm_of_ocaml/wasm_of_ocaml.exe"
    "compiler/bin-wasm_of_ocaml/wasmoo_link_wasm.exe"
  ];
in
rec {
  vendorSrc =
    let
      op = ocamlPackages;
    in
    pkgs.runCommand "jsoo-vendor-src" { } ''
      mkdir -p $out
      printf '(vendored_dirs *)\n' > $out/dune

      # cmdliner has no dune build; give the library a minimal dune port.
      mkdir $out/cmdliner
      tar xf ${op.cmdliner.src} -C $out/cmdliner --strip-components=1
      printf '(lang dune 3.0)\n(name cmdliner)\n(package (name cmdliner))\n' \
        > $out/cmdliner/dune-project
      printf '(library\n (name cmdliner)\n (public_name cmdliner)\n (wrapped false))\n' \
        > $out/cmdliner/src/dune

      mkdir $out/yojson
      tar xf ${op.yojson.src} -C $out/yojson --strip-components=1
      # These aliases inherit the OxCaml stdlib's `@ local` argument modes,
      # which the unannotated mlis reject; eta-expand.
      sed -i 's/^let write_\(intlit\|floatlit\|stringlit\) = Buffer.add_string$/let write_\1 ob s = Buffer.add_string ob s/' \
        $out/yojson/lib/write.ml

      # Runtime library only; the ppx half lives in the jsoo_ppx driver.
      cp -r ${op.sedlex.src} $out/sedlex
      chmod -R u+w $out/sedlex
      rm -rf $out/sedlex/src/syntax

      # The seq compat shim is part of the stdlib since OCaml 4.07.
      cp -r ${op.gen.src} $out/gen
      chmod -R u+w $out/gen
      sed -i 's/(libraries seq)//' $out/gen/src/dune

      # OxCaml cma unit names are Compilation_unit values, not plain strings.
      cp -r ${op.ocaml-compiler-libs.src} $out/ocaml-compiler-libs
      chmod -R u+w $out/ocaml-compiler-libs
      sed -i 's/^let compunit_name Cmo_format.{ cu_name = Compunit name ; _ } = name$/let compunit_name cu = cu.Cmo_format.cu_name |> Compilation_unit.name |> Compilation_unit.Name.to_string/' \
        $out/ocaml-compiler-libs/src/read_cma/read_cma.ml

      ln -s ${menhirSrc} $out/menhir
    '';

  ppxDriver = pkgs.stdenv.mkDerivation {
    pname = "jsoo-ppx-driver";
    inherit version;

    # Restricted to what the driver needs so js_of_ocaml edits don't rebuild it.
    src = lib.fileset.toSource {
      root = subtree;
      fileset = lib.fileset.unions [
        (subtree + "/dune-project")
        (subtree + "/compiler/bin-jsoo_ppx")
        (subtree + "/compiler/ppx-optcomp-light")
        (subtree + "/compiler/ppx-light-predicate")
      ];
    };

    nativeBuildInputs = [
      dune
      ocamlPackages.ocaml
      ocamlPackages.findlib
      pkgs.makeWrapper
    ];
    buildInputs = with ocamlPackages; [
      ppxlib
      sedlex
    ];

    buildPhase = ''
      runHook preBuild
      export HOME="$TMPDIR"
      dune build compiler/bin-jsoo_ppx/jsoo_ppx.exe
      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall
      install -Dm755 _build/default/compiler/bin-jsoo_ppx/jsoo_ppx.exe $out/bin/jsoo_ppx
      wrapProgram $out/bin/jsoo_ppx --set-default JSOO_PPX_OCAML_VERSION ${
        lib.head (lib.splitString "\n" (lib.fileContents ./VERSION))
      }
      runHook postInstall
    '';
  };

  shellInputs = [
    ppxDriver
    pkgs.binaryen
    pkgs.nodejs_latest
  ];

  # Hermetic build of the js_of_ocaml/wasm_of_ocaml binaries. Excluding the
  # js_of_ocaml subtree and the nix files from the compiler source means
  # editing them does not rebuild the compiler.
  binaries =
    let
      oxcaml = import ./default.nix {
        inherit pkgs;
        withJsoo = false;
        src = lib.cleanSourceWith {
          name = "oxcaml-src";
          src = lib.cleanSource ./.;
          filter =
            path: _type:
            !(lib.any (suffix: lib.hasSuffix suffix path) [
              "/external/js_of_ocaml"
              "/jsoo.nix"
              "/default.nix"
              "/flake.nix"
              "/flake.lock"
              "/_build"
              "/_install"
              "/_runtest"
              "/_coverage"
              "/_profile"
            ]);
        };
      };
    in
    pkgs.stdenv.mkDerivation {
      pname = "jsoo-oxcaml";
      inherit version;

      src = lib.cleanSourceWith {
        name = "jsoo-src";
        src = lib.cleanSource subtree;
        filter =
          path: _type:
          !(lib.any (suffix: lib.hasSuffix suffix path) [
            "/_build"
            "/vendor"
          ]);
      };

      nativeBuildInputs = [
        dune
        oxcaml
        ppxDriver
        pkgs.binaryen
        pkgs.makeWrapper
      ];

      buildPhase = ''
        runHook preBuild
        export HOME="$TMPDIR"
        ln -s ${vendorSrc} vendor
        dune build --profile release ${lib.concatStringsSep " " targets}
        runHook postBuild
      '';

      installPhase = ''
        runHook preInstall
        for exe in ${lib.concatStringsSep " " targets}; do
          install -Dm755 "_build/default/$exe" "$out/bin/$(basename "$exe" .exe)"
        done
        wrapProgram $out/bin/wasm_of_ocaml --prefix PATH : ${pkgs.binaryen}/bin
        runHook postInstall
      '';
    };
}
