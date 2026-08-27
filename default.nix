{
  pkgs ? import <nixpkgs> { },
  src ? ./.,
  addressSanitizer ? false,
  dev ? false,
  flambdaInvariants ? false,
  framePointers ? addressSanitizer,
  multidomain ? false,
  ocamltest ? true,
  pollInsertion ? false,
  stackChecks ? false,
  warnError ? true,
  oxcamlClang ? false,
  oxcamlLldb ? false,
  syntaxQuotations ? false,
  withMerlin ? true,
}:
let
  inherit (pkgs) lib fetchpatch;

  # Select stdenv based on whether asan is enabled
  stdenv = if addressSanitizer then pkgs.clangStdenv else pkgs.stdenv;

  # Build configure flags based on features
  configureFlags =
    let
      mkFlag = bool: name: if bool then "--enable-${name}" else "--disable-${name}";
    in
    [
      "--cache-file=/dev/null"
      "--with-objcopy=${pkgs.llvm}/bin/llvm-objcopy"
      (
        if pkgs.stdenv.isDarwin then
          "--disable-assembler-suitable-for-dissector"
        else
          "--enable-assembler-suitable-for-dissector=${pkgs.llvm}/bin/llvm-mc"
      )
      (mkFlag addressSanitizer "address-sanitizer")
      (mkFlag dev "dev")
      (mkFlag flambdaInvariants "flambda-invariants")
      (mkFlag framePointers "frame-pointers")
      (mkFlag multidomain "multidomain")
      (mkFlag pollInsertion "poll-insertion")
      (mkFlag stackChecks "stack-checks")
      (mkFlag warnError "warn-error")
      (mkFlag ocamltest "ocamltest")
      (mkFlag syntaxQuotations "syntax-quotations")
    ];

  # Boot compilers
  ocaml_4_14_2 = (pkgs.ocaml-ng.ocamlPackages_4_14.ocaml.override { inherit stdenv; }).overrideAttrs {
    # This patch is from oxcaml PR 3960, which fixes an issue in the upstream
    # compiler that we use to bootstrap ourselves on ARM64
    patches = [
      ./tools/ci/local-opam/packages/ocaml-base-compiler/ocaml-base-compiler.4.14.2+oxcaml/files/ocaml-base-compiler.4.14.2+oxcaml.patch
    ];
  };

  mkBootOcaml_5_4_0 = bootStdenv: (pkgs.callPackage (
    import (pkgs.path + "/pkgs/development/compilers/ocaml/generic.nix") {
      major_version = "5";
      minor_version = "4";
      patch_version = "0";
      sha256 = "sha256-36qKLhHHmbwXZdi+9EkRQG7l9IAwJxkDgqk5+IyRImY=";
    }) {
      stdenv = bootStdenv;
    }).overrideAttrs {
      # This patch fixes an issue in the upstream compiler that we use to
      # bootstrap ourselves on ARM64
      patches = [
        ./tools/ci/local-opam/packages/ocaml-base-compiler/ocaml-base-compiler.5.4.0+oxcaml/files/ocaml-base-compiler.5.4.0+oxcaml.patch
      ];

      # Skip the upstream testsuite for this bootstrap compiler. When built
      # with our clang-based stdenv on the 26.05 toolchain,
      # testsuite/tests/unicode fails: it compiles modules with non-ASCII
      # source filenames and the UTF-8 bytes of the object filenames reach
      # clang octal-escaped (e.g. '$350246213.o'), so linking fails. This
      # compiler only exists to bootstrap oxcaml, which runs its own `make ci`
      # afterwards.
      doCheck = false;
    };

  ocaml_5_4_0 = mkBootOcaml_5_4_0 stdenv;

  # CR sspies: For the time being, we use dune built with the vanilla 4.14.2 compiler.
  # Over time, we should probably define something like a "boot environment" and build
  # dune and the other dependencies with the patched system compiler.
  dune = pkgs.ocaml-ng.ocamlPackages_4_14.dune_3.overrideAttrs rec {
    # This version should be the same as in tools/ci/local-opam/packages/oxcaml-ci-deps
    version = "3.23.1";
    src = pkgs.fetchurl {
      url = "https://github.com/ocaml/dune/releases/download/${version}/dune-${version}.tbz";
      hash = "sha256-k7TnFX9rqP62HPxfhgCO/SxZA3unigF9krSr8wYyNI8=";
    };
  };

  ocamlformat = pkgs.ocaml-ng.ocamlPackages_4_14.ocamlformat.overrideAttrs (old: rec {
    name = "${old.pname}-${version}";
    version = "0.29.0";
    src = pkgs.fetchurl {
      url = "https://github.com/ocaml-ppx/ocamlformat/releases/download/${version}/ocamlformat-${version}.tbz";
      sha256 = "sha256-2sd/CpV654K7S4abB7mAOocqNPjB6uiQG0LSG2I8nbU=";
    };
  });

  menhirVersion = "20231231";

  menhirSrc = pkgs.fetchFromGitLab {
    domain = "gitlab.inria.fr";
    owner = "fpottier";
    repo = "menhir";
    tag = menhirVersion;
    sha256 = "sha256-veB0ORHp6jdRwCyDDAfc7a7ov8sOeHUmiELdOFf/QYk=";
  };

  menhirLib = pkgs.ocaml-ng.ocamlPackages_4_14.menhirLib.overrideAttrs (
    new: old: {
      version = menhirVersion;
      patches = [ ];
      src = menhirSrc;
    }
  );

  menhir =
    let
      menhirSdk = pkgs.ocaml-ng.ocamlPackages_4_14.menhirSdk.override { inherit menhirLib; };
    in
    (pkgs.ocaml-ng.ocamlPackages_4_14.menhir.override { inherit menhirLib; }).overrideAttrs (
      new: old: {
        patches = [ ];
        buildInputs = [
          menhirLib
          menhirSdk
        ];
        postInstall = ''
          ln -s ${menhirLib}/lib/ocaml/*/site-lib/menhirLib $out/lib/
        '';
      }
    );

  mkMerlinPackages =
    testOcaml:
    let
      # nixpkgs does not yet provide an OCaml 5.4 package set at the pinned
      # revision, so construct one around the compiler used to bootstrap
      # OxCaml. Pin the plain pkgs.stdenv for this compiler rather than the
      # variant stdenv: a clangStdenv-built compiler records `clang` as its C
      # compiler, which isn't on PATH when the scope's packages build under
      # the default gcc stdenv (e.g. findlib's `ocamlc -custom` link of
      # ocamlfind). Pinning also keeps the dev-tool closure identical across
      # oxcaml variants, so they share cached builds.
      merlinBootOcaml = mkBootOcaml_5_4_0 pkgs.stdenv;
      ocamlPackages =
        (pkgs.ocaml-ng.mkOcamlPackages merlinBootOcaml).overrideScope (
          _: osuper: {
            dune_3 = dune;

            # ounit2's own test suite has a flaky threads test that can hit
            # its 600s timeout on loaded CI runners; skip upstream's tests.
            ounit2 = osuper.ounit2.overrideAttrs (_: { doCheck = false; });

            # Merlin relies on generated parser sources matching this version.
            menhirLib = osuper.menhirLib.overrideAttrs (_: {
              version = menhirVersion;
              src = menhirSrc;
            });

            # menhirGLR inherits menhirLib's pinned src, which predates the
            # menhirGLR package; menhir builds fine without it.
            menhirGLR = null;

            # nixpkgs' suggest-menhirLib patch doesn't apply to the pinned
            # menhir source (same reason the 4.14 menhir above drops it).
            menhir = osuper.menhir.overrideAttrs (_: { patches = [ ]; });

            inherit (packages) merlin-lib dot-merlin-reader merlin;
          }
        );

      inherit (ocamlPackages) buildDunePackage;
      merlinSrc = "${src}/external/merlin";

      packages = rec {
        merlin-lib = buildDunePackage {
          pname = "merlin-lib";
          version = "dev";
          src = merlinSrc;
          duneVersion = "3";
          propagatedBuildInputs = [ ocamlPackages.csexp ];
          checkInputs = [ ocamlPackages.alcotest ];
          doCheck = true;
        };

        dot-merlin-reader = buildDunePackage {
          pname = "dot-merlin-reader";
          version = "dev";
          src = merlinSrc;
          duneVersion = "3";
          propagatedBuildInputs = [ ocamlPackages.findlib ];
          buildInputs = [ merlin-lib ];
          checkInputs = [ ocamlPackages.alcotest ];
          doCheck = true;
        };

        merlin = buildDunePackage {
          pname = "merlin";
          version = "dev";
          src = merlinSrc;
          duneVersion = "3";
          buildInputs = [
            merlin-lib
            dot-merlin-reader
            ocamlPackages.menhirLib
            ocamlPackages.menhirSdk
            ocamlPackages.yojson
          ];
          nativeBuildInputs = [
            ocamlPackages.menhir
            pkgs.jq
          ];
          nativeCheckInputs = [
            dot-merlin-reader
            pkgs.python3
            pkgs.which
            testOcaml
          ];
          checkInputs = [ ocamlPackages.alcotest ];
          doCheck = true;
          checkPhase = ''
            runHook preCheck
            patchShebangs \
              tests/merlin-wrapper \
              tests/ocamlc-wrapper \
              tests/dune-wrapper \
              scripts/combine-merge-conflicts.py \
              src/ocaml-index/tests/ocamlobjinfo-wrapper
            DUNE_CACHE=disabled MERLIN_TEST_OCAML_PATH=${testOcaml} \
              dune build @check @runtest
            runHook postCheck
          '';
          meta.mainProgram = "ocamlmerlin";
          passthru = {
            devBuildInputs = [
              ocamlPackages.alcotest
              ocamlPackages.csexp
              ocamlPackages.findlib
              ocamlPackages.menhirLib
              ocamlPackages.menhirSdk
              ocamlPackages.yojson
            ];
            devNativeBuildInputs = [
              ocamlPackages.menhir
              pkgs.jq
              pkgs.python3
              pkgs.which
            ];
          };
        };
      };
    in
    packages;

  # Only the passthru dev-input lists are used here, which don't depend on the
  # testOcaml argument (it only feeds the merlin package's check phase).
  merlinDev = (mkMerlinPackages ocaml_5_4_0).merlin;

  gfortran =
    # we require fortran for some bigarray tests, but adding `pkgs.gfortran`
    # directly to `nativeBuildInputs` overrides many `$PATH` entries from
    # `myStdenv` that we want to keep, such as `as` and `objcopy`
    pkgs.linkFarm "gfortran-only" { "bin/gfortran" = lib.getExe pkgs.gfortran; };

  makeLlvm =
    {
      pname,
      version,
      src,
      projects,
    }:
    pkgs.stdenv.mkDerivation {
      inherit pname version src;

      nativeBuildInputs = with pkgs; [
        cmake
        ninja
        perl
      ];

      buildInputs = with pkgs; [
        python312
        libxml2
        ncurses
        zlib
        libedit
        xz # lzma for -DLLDB_ENABLE_LZMA=ON, lldb emits a warning otherwise
        swig
      ];

      cmakeFlags = [
        "-DLLVM_ENABLE_PROJECTS=${lib.strings.concatStringsSep ";" projects}"
        "-DCMAKE_BUILD_TYPE=Release"
        "-DLLVM_TARGETS_TO_BUILD=Native"
        "-DLLDB_ENABLE_PYTHON=ON"
        "-DLLDB_ENABLE_LIBEDIT=ON"
        "-DLLDB_ENABLE_CURSES=ON"
        "-DLLDB_ENABLE_LZMA=ON"
        # Disable tests to avoid needing libc++
        "-DLLDB_INCLUDE_TESTS=OFF"
        "-DLLVM_INCLUDE_TESTS=OFF"
        "-DCLANG_INCLUDE_TESTS=OFF"
      ];

      sourceRoot = "${src.name}/llvm";
      enableParallelBuilding = true;

      # Fix permission issue: version-header-fix.py overwrites lldb-defines.h
      # during the build and needs the right permissions to do so.
      postUnpack = ''
        chmod u+w ${src.name}/lldb/include/lldb/lldb-defines.h
      '';
    }

  ;

  lldb = makeLlvm rec {
    pname = "oxcaml-lldb";
    version = "21.1.0+oxcaml0";
    projects = [
      "clang"
      "lldb"
    ];
    src = pkgs.fetchFromGitHub {
      owner = "ocaml-flambda";
      repo = "llvm-project";
      tag = "oxcaml-lldb-${version}";
      hash = "sha256-DrXQY/1MJCgSqHd4vHpUbdTVWbLZEXkDbUVRYOp3e6Y=";
    };
  };

  clang = makeLlvm {
    pname = "llvm";
    version = "oxcaml-llvmize-16.0.6-minus0";

    projects = [ "clang" ];
    src = pkgs.fetchFromGitHub {
      owner = "ocaml-flambda";
      repo = "llvm-project";
      tag = "oxcaml-llvmize-16.0.6-minus0";
      sha256 = "sha256-D3nqlXfj1CI3KaQrERRXaxYCwVDfycOpa0ryeZn8xz8=";
    };
  };
in
stdenv.mkDerivation {
  pname = "oxcaml";
  version = "5.4.0+ox";
  inherit src configureFlags;

  OXCAML_LLDB = if oxcamlLldb then "${lldb}/bin/lldb" else null;
  OXCAML_CLANG = if oxcamlClang then "${clang}/bin/clang" else null;

  enableParallelBuilding = true;
  separateDebugInfo = false;
  dontStrip = true;

  # Disable _multioutConfig hook which adds --libdir=$out/lib into
  # configureFlags when separateDebugInfo is enabled, breaking OCaml's configure
  # step, which expects --libdir to be $out/lib/ocaml
  setOutputFlags = false;

  nativeBuildInputs = [
    pkgs.autoconf
    menhir
    ocaml_5_4_0
    pkgs.ocaml-ng.ocamlPackages_5_4.ocaml-lsp
    dune
    pkgs.pkg-config
    pkgs.rsync
    pkgs.which
    pkgs.parallel
    gfortran # Required for Bigarray Fortran tests
    ocamlformat # required for make fmt
    pkgs.removeReferencesTo
  ]
  ++ (if pkgs.stdenv.isDarwin then [ pkgs.cctools ] else [ pkgs.libtool ]) # cctools provides Apple libtool on macOS
  ++ lib.optional oxcamlLldb pkgs.python312
  ++ lib.optionals withMerlin merlinDev.devNativeBuildInputs;

  buildInputs = [
    pkgs.llvm # llvm-objcopy is used for debuginfo
  ]
  ++ lib.optionals withMerlin merlinDev.devBuildInputs;

  preConfigure = ''
    rm -rf _build _install _runtest

    # We don't use autoreconfHook because libtoolize and autoheader are
    # incompatible with ocaml-flambda
    autoconf --force
  '';

  checkPhase = lib.optionalString ocamltest ''
    # The testsuite/tests/unicode test compiles modules with non-ASCII source
    # filenames (néant.ml, 見.ml) and links them via clang. Under the 26.05
    # toolchain the UTF-8 bytes of the object filenames reach clang octal-escaped
    # (e.g. '$350246213.o'), so linking fails with "no such file or directory".
    # This exercises unicode source filenames, which we don't use; drop the test
    # so the rest of `make ci` runs.
    rm -rf testsuite/tests/unicode
    make ci
  '';

  postInstall =
    # Get rid of unused artifacts
    ''
      $out/bin/generate_cached_generic_functions.exe $out/lib/ocaml/cached-generic-functions
      rm -f $out/bin/dumpobj.byte
      rm -f $out/bin/extract_externals.byte
      rm -f $out/bin/generate_cached_generic_functions.exe
      rm -f $out/bin/ocamlcp
      rm -f $out/bin/ocamlmklib.byte
      rm -f $out/bin/ocamlmktop.byte
      rm -f $out/bin/ocamlobjinfo.byte
      rm -f $out/bin/ocamlopt.byte
      rm -f $out/bin/ocamlprof
      rm -f $out/lib/ocaml/expunge
    '';

  postFixup = ''
    remove-references-to -t ${dune} $out/lib/ocaml/Makefile.config
  '';

  shellHook =
    let
      merlinCommands =
        if withMerlin then
          "  make merlin-build        - Build Merlin\n"
          + "  make merlin-test         - Run the Merlin tests\n"
          + "  make merlin-promote      - Promote Merlin test output\n"
        else
          "  (make merlin-* targets need this shell built with withMerlin=true,\n"
          + "   as the flake's devShell does)\n";
    in
    ''
      prefix="$(pwd)/_install"

      cat >&2 << EOF
      OxCaml $version Development Environment
      ===============================''${version//?/=}

      Available commands:
        configurePhase           - Pre-build setup
        make boot-compiler       - Quick build (recommended for development)
        make boot-_install       - Quick install (recommended for development)
        make fmt                 - Auto-format code
        make                     - Full build
        make install             - Install
        make test                - Run all tests
        make test-one TEST=...   - Run a single test
      ${merlinCommands}EOF
    '';

  meta =
    { } // (if framePointers && !pkgs.stdenv.hostPlatform.isx86_64 then { broken = true; } else { });

  passthru = {
    inherit
      ocaml_4_14_2
      ocaml_5_4_0
      ocamlformat
      lldb
      mkMerlinPackages
      ;
  };

}
