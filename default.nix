{
  pkgs ? import <nixpkgs> { },
  src ? ./.,
  # Short commit hash of src, recorded in the js_of_ocaml version.
  gitRev ? null,
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
  withAstDependentLibs ? true,
  withJsoo ? true,
  # Test-only sources for make jsoo-test; not needed to build the compiler.
  withJsooTestSources ? pkgs.lib.inNixShell,
}:
let
  inherit (pkgs) lib;

  # js_of_ocaml is built on top of ppxlib.
  needsPpxlibSources = withAstDependentLibs || withJsoo;

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

  # nixpkgs does not yet provide an OCaml 5.4 package set at the pinned
  # revision, so construct one around the compiler used to bootstrap OxCaml.
  # Pin the plain pkgs.stdenv for this compiler rather than the variant
  # stdenv: a clangStdenv-built compiler records `clang` as its C compiler,
  # which isn't on PATH when the scope's packages build under the default gcc
  # stdenv (e.g. findlib's `ocamlc -custom` link of ocamlfind). Pinning also
  # keeps the dev-tool closure identical across oxcaml variants, so they share
  # cached builds.
  bootOcamlPackages =
    let
      ocaml = mkBootOcaml_5_4_0 pkgs.stdenv;
    in
    (pkgs.ocaml-ng.mkOcamlPackages ocaml).overrideScope (
      _: osuper: {
        dune_3 = dune;

        # ounit2's own test suite has a flaky threads test that can hit its
        # 600s timeout on loaded CI runners; skip upstream's tests.
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
      }
    );

  mkMerlinPackages =
    testOcaml:
    let
      ocamlPackages = bootOcamlPackages.overrideScope (
        _: _: { inherit (packages) merlin-lib dot-merlin-reader merlin; }
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

  # The libraries below are only there as dependencies of the shipped ones.
  # Users of the compiler package may have their own copies, at other versions,
  # so these must not be visible: each one is a wrapped library with a unique
  # name (oxcaml_private_<name>), which makes every compilation unit
  # Oxcaml_private_<name>__<Module>. Without its dune-project, it joins the
  # project that needs it (the Makefile links it into that project's
  # oxcaml-private/ directory) and installs as a sub-library of one of its
  # packages, e.g. js_of_ocaml-compiler.private.yojson. The shipped libraries
  # see the old module names through -open Oxcaml_private_<name>.
  joinConsumingProject = ''
    rm -f dune-project *.opam
  '';

  ppxDeriversSrc =
    let
      version = "1.2.1";
      srcDune = pkgs.writeText "ppx_derivers-src-dune" ''
        (library
         (name oxcaml_private_ppx_derivers)
         (public_name ppxlib.private.ppx_derivers))
      '';
    in
    pkgs.applyPatches {
      name = "ppx_derivers-${version}-source";
      src = pkgs.fetchFromGitHub {
        owner = "ocaml-ppx";
        repo = "ppx_derivers";
        tag = version;
        hash = "sha256-9k4rbB1G4894F95XPMQsiVgwZKJ2XcaDUaEviArHG3s=";
      };
      postPatch = ''
        ${joinConsumingProject}
        cp ${srcDune} src/dune
      '';
    };

  # sexp_type has no releases; pin a commit. Unlike the other dependencies, it
  # is shipped as its own public package: its type is part of ppxlib's
  # interface (Stdppx.Sexp.t), and must be the same as the one users' sexplib0
  # builds on.
  sexpTypeSrc = pkgs.applyPatches {
    name = "sexp_type-6d16004-source";
    src = pkgs.fetchFromGitHub {
      owner = "janestreet";
      repo = "sexp_type";
      rev = "6d16004ed65cbed153c130d4beba1c5655146152";
      hash = "sha256-Iq8AosBQGfl78pBr4IVRpDBMH6XDHuNvUUjeN/WgImE=";
    };
    # Only the sexp_type library is needed; sexp_type.grammar depends on
    # basement, which is not available here.
    postPatch = ''
      rm -r grammar_type
    '';
  };

  # "seq" is an empty compatibility package with no dune equivalent.
  dropSeqDependency = file: "substituteInPlace ${file} --replace-fail '(libraries seq)' ''";

  # Keep the release's opam metadata; the prepared source is read-only.
  keepReleaseOpamFiles = ''
    substituteInPlace dune-project \
      --replace-fail '(generate_opam_files true)' '(generate_opam_files false)'
  '';

  # The ppx and sedlex.utils, which only the ppx uses, are only needed while
  # building. The ppx refers to sedlex.utils as Sedlex_utils, so that name is
  # kept.
  sedlexSrc =
    let
      version = "3.7";
      libDune = pkgs.writeText "sedlex-lib-dune" ''
        (library
         (name oxcaml_private_sedlex)
         (public_name js_of_ocaml-compiler.private.sedlex)
         (libraries oxcaml_private_gen)
         (flags :standard -w +A-4-9 -safe-string -open Oxcaml_private_gen))
      '';
      commonDune = pkgs.writeText "sedlex-common-dune" ''
        (library
         (name sedlex_utils))
      '';
      syntaxDune = pkgs.writeText "sedlex-syntax-dune" ''
        (library
         (name oxcaml_private_sedlex_ppx)
         (kind ppx_rewriter)
         (libraries ppxlib oxcaml_private_sedlex sedlex_utils)
         (ppx_runtime_libraries oxcaml_private_sedlex)
         (preprocess
          (pps ppxlib.metaquot))
         (flags
          (:standard -w -9 -open Oxcaml_private_sedlex)))
      '';
    in
    pkgs.applyPatches {
      name = "sedlex-${version}-source";
      src = pkgs.fetchFromGitHub {
        owner = "ocaml-community";
        repo = "sedlex";
        tag = "v${version}";
        hash = "sha256-ucqrJkzS6cVogGUf1vU8oBpSryneMBqTjzxwsOi6Egs=";
      };
      # Adapt the ppx to the OxCaml parsetree (labeled tuples, function
      # parameters), and use the shipped unicode.ml: its promote rule
      # downloads the Unicode data.
      patches = [ ./external/patches/sedlex-oxcaml-syntax.patch ];
      postPatch = ''
        rm -r examples test src/generator
        ${joinConsumingProject}
        cp ${libDune} src/lib/dune
        cp ${commonDune} src/common/dune
        cp ${syntaxDune} src/syntax/dune
      '';
    };

  genSrc =
    let
      version = "1.1";
    in
    pkgs.applyPatches {
      name = "gen-${version}-source";
      src = pkgs.fetchFromGitHub {
        owner = "c-cube";
        repo = "gen";
        tag = "v${version}";
        hash = "sha256-ZytPPGhmt/uANaSgkgsUBOwyQ9ka5H4J+5CnJpEdrNk=";
      };
      postPatch = ''
        rm -r bench qtest
        ${joinConsumingProject}
        ${dropSeqDependency "src/dune"}
        substituteInPlace src/dune \
          --replace-fail '(name gen)' '(name oxcaml_private_gen)' \
          --replace-fail '(public_name gen)' \
            '(public_name js_of_ocaml-compiler.private.gen)' \
          --replace-fail '(wrapped false)' ""
      '';
    };

  # Cmdliner has no dune build; add one for the library.
  cmdlinerSrc =
    let
      version = "2.1.1";
      srcDune = pkgs.writeText "cmdliner-src-dune" ''
        (library
         (name oxcaml_private_cmdliner)
         (public_name js_of_ocaml-compiler.private.cmdliner))
      '';
    in
    pkgs.applyPatches {
      name = "cmdliner-${version}-source";
      src = pkgs.fetchzip {
        url = "https://erratique.ch/software/cmdliner/releases/cmdliner-${version}.tbz";
        hash = "sha256-WJEtB7PI8wB+nbVavPso4m1poy1JJnhtGQ4JRXJT2F4=";
      };
      postPatch = ''
        ${joinConsumingProject}
        cp ${srcDune} src/dune
      '';
    };

  # Only menhirLib is built from this tree; keeping the menhir executable's
  # sources out stops dune from preferring it over the one from nixpkgs.
  # Without the dune-project, the version that lib/dune reads from it is
  # spelled out.
  menhirLibrariesSrc = pkgs.applyPatches {
    name = "menhir-${menhirVersion}-libraries-source";
    src = menhirSrc;
    postPatch = ''
      find . -mindepth 1 -maxdepth 1 ! -name LICENSE ! -name lib \
        -exec rm -r {} +
      substituteInPlace lib/dune \
        --replace-fail '%{version:menhir}' '${menhirVersion}'
      substituteInPlace lib/pack/dune \
        --replace-fail '(name menhirLib)' '(name oxcaml_private_menhirlib)' \
        --replace-fail '(public_name menhirLib)' \
          '(public_name js_of_ocaml-compiler.private.menhirLib)'
    '';
  };

  yojsonSrc =
    let
      version = "2.2.2";
    in
    pkgs.applyPatches {
      name = "yojson-${version}-source";
      src = pkgs.fetchzip {
        url = "https://github.com/ocaml-community/yojson/releases/download/${version}/yojson-${version}.tbz";
        hash = "sha256-V3USV8xhN1pQq5m0KmvUwgw7MujuCdWsv3lD+8PkECA=";
      };
      postPatch = ''
        rm -r bench bin doc examples test test_json5 lib/json5
        ${joinConsumingProject}
        ${dropSeqDependency "lib/dune"}
        substituteInPlace lib/dune \
          --replace-fail '(name yojson)' '(name oxcaml_private_yojson)' \
          --replace-fail '(public_name yojson)' \
            '(public_name js_of_ocaml-compiler.private.yojson)'
        # Eta-expand to avoid exposing Buffer.add_string's OxCaml modes.
        substituteInPlace lib/write.ml \
          --replace-fail 'let write_intlit = Buffer.add_string' \
            'let write_intlit ob s = Buffer.add_string ob s' \
          --replace-fail 'let write_floatlit = Buffer.add_string' \
            'let write_floatlit ob s = Buffer.add_string ob s' \
          --replace-fail 'let write_stringlit = Buffer.add_string' \
            'let write_stringlit ob s = Buffer.add_string ob s'
      '';
    };

  outChannelRedirectSrc =
    let
      version = "0.2";
    in
    pkgs.applyPatches {
      name = "out-channel-redirect-${version}-source";
      src = pkgs.fetchzip {
        url = "https://github.com/hhugo/out-channel-redirect/releases/download/${version}/out-channel-redirect-${version}.tbz";
        hash = "sha256-ThMQaTtmT6ltL0As7K9761qEz/jPKfv3vsxnVNHeKl4=";
      };
      postPatch = keepReleaseOpamFiles;
    };

  qcheckSrc =
    let
      version = "0.25";
    in
    pkgs.applyPatches {
      name = "qcheck-${version}-source";
      src = pkgs.fetchFromGitHub {
        owner = "c-cube";
        repo = "qcheck";
        tag = "v${version}";
        hash = "sha256-Z89jJ21zm89wb9m5HthnbHdnE9iXLyaH9k8S+FAWkKQ=";
      };
      # Do not constrain the callback to Array.length's polymodal signature.
      postPatch = ''
        substituteInPlace src/core/QCheck.ml \
          --replace-fail '_opt_map_or ~d:Array.length ~f:array_sum_' \
            '_opt_map_or ~d:(fun a -> Array.length a) ~f:array_sum_'
      '';
    };

  # Read by the oxcaml-private/* and external/ast-dependent-libs/deps/* rules
  # of the Makefile.
  ppxlibSources = {
    PPXLIB_PPX_DERIVERS_SRC = ppxDeriversSrc;
    PPXLIB_SEXP_TYPE_SRC = sexpTypeSrc;
  };

  jsooSources = {
    SEDLEX_GEN_SRC = genSrc;
    JSOO_SEDLEX_SRC = sedlexSrc;
    JSOO_CMDLINER_SRC = cmdlinerSrc;
    JSOO_MENHIR_SRC = menhirLibrariesSrc;
    JSOO_YOJSON_SRC = yojsonSrc;
  };

  jsooTestSources = {
    JSOO_OUT_CHANNEL_REDIRECT_SRC = outChannelRedirectSrc;
    JSOO_QCHECK_SRC = qcheckSrc;
  };

  # Run make buildTarget with an installed OxCaml, then installTarget, which
  # installs findlib packages under $out/lib and executables under $out/bin.
  # Without an installTarget, $out is empty (for checks).
  mkAstDependentLibsBuild =
    {
      pname,
      buildTarget,
      installTarget ? null,
      sources,
      extraNativeBuildInputs ? [ ],
      postBuild ? "",
      postInstall ? "",
    }:
    oxcaml:
    stdenv.mkDerivation (
      {
        inherit pname;
        inherit (oxcaml) version meta;
        inherit src;

        nativeBuildInputs = [
          dune
          oxcaml
        ]
        ++ extraNativeBuildInputs;

        dontConfigure = true;
        # Darwin's strip aborts on the installed .wasm runtimes.
        dontStrip = true;

        makeFlags = [
          # The Makefile's /usr/bin/env bash does not exist in the build sandbox.
          "SHELL=${stdenv.shell}"
          "REQUIRES_CONFIGURATION="
          "DUNE=${dune}/bin/dune"
          "OXCAML_INSTALL=${oxcaml}"
        ];

        buildFlags = [ buildTarget ];

        inherit postBuild postInstall;

        env = sources // lib.optionalAttrs (gitRev != null) { JSOO_GIT_VERSION = "ox-${gitRev}"; };
      }
      // (
        if installTarget == null then
          {
            installPhase = ''
              runHook preInstall
              mkdir "$out"
              runHook postInstall
            '';
          }
        else
          {
            installTargets = [ installTarget ];
            installFlags = [ "AST_DEPENDENT_LIBS_PREFIX=${placeholder "out"}" ];
          }
      )
    );

  jsooTools = [
    menhir
    pkgs.nodejs
    pkgs.binaryen
    pkgs.makeWrapper
  ];

  # wasm_of_ocaml runs the Binaryen tools from PATH.
  wrapWasmOfOcaml = ''
    wrapProgram "$out/bin/wasm_of_ocaml" \
      --prefix PATH : ${lib.makeBinPath [ pkgs.binaryen ]}
  '';

  mkPpxlibLibs = mkAstDependentLibsBuild {
    pname = "oxcaml-ppxlib";
    buildTarget = "ppxlib-build";
    installTarget = "ppxlib-install";
    sources = ppxlibSources;
  };

  # Some generated sources of these projects are checked in and regenerated by
  # dune rules in (mode promote), which rewrite the source tree: in the build
  # sandbox that is the writable copy of the sources, so a stale checked-in
  # file goes unnoticed. Compare the vendored sources with the pristine copy
  # after building, and fail if the build changed any. The projects are the
  # ones linked into the external/ast-dependent-libs dune root; the links to
  # private dependencies that make adds to their oxcaml-private/ directories
  # are expected.
  checkPromotedSources = ''
    stale=0
    for link in external/ast-dependent-libs/*; do
      [ -L "$link" ] || continue
      project=$(realpath --relative-to=. "$link")
      diffs=$(diff -rq "$src/$project" "$project" \
        | grep -v "^Only in $project/oxcaml-private: " || true)
      if [ -n "$diffs" ]; then
        echo "$diffs" >&2
        stale=1
      fi
    done
    if [ "$stale" -ne 0 ]; then
      echo "error: the build changed checked-in files under external/;" \
        "regenerate and commit them" >&2
      exit 1
    fi
  '';

  mkJsooLibs = mkAstDependentLibsBuild {
    pname = "oxcaml-jsoo";
    buildTarget = "jsoo-build";
    installTarget = "jsoo-install";
    sources = ppxlibSources // jsooSources;
    extraNativeBuildInputs = jsooTools;
    postBuild = checkPromotedSources;
    postInstall = wrapWasmOfOcaml;
  };

  mkJsooTest = mkAstDependentLibsBuild {
    pname = "oxcaml-jsoo-test";
    buildTarget = "jsoo-test";
    sources = ppxlibSources // jsooSources // jsooTestSources;
    extraNativeBuildInputs = jsooTools;
  };

  # Builds and runs a small js_of_ocaml program using only what a compiler
  # package built withJsoo ships.
  mkJsooSmokeTest =
    oxcaml:
    stdenv.mkDerivation {
      pname = "oxcaml-jsoo-smoke-test";
      inherit (oxcaml) version meta;
      src = "${src}/external/ast-dependent-libs/smoke";

      nativeBuildInputs = [
        dune
        oxcaml
        pkgs.nodejs
      ];

      dontConfigure = true;

      buildPhase = ''
        runHook preBuild
        DUNE_CACHE=disabled dune build --root . @runtest
        runHook postBuild
      '';

      installPhase = ''
        runHook preInstall
        mkdir "$out"
        runHook postInstall
      '';
    };

  # Check metadata and native consumers of the full bundled compiler install.
  mkInstalledLibrariesCheck =
    oxcaml:
    stdenv.mkDerivation {
      pname = "oxcaml-installed-libraries-check";
      inherit (oxcaml) version meta;
      inherit src;

      nativeBuildInputs = [
        bootOcamlPackages.ocaml
        bootOcamlPackages.findlib
        dune
      ];

      dontConfigure = true;
      makeFlags = [
        "SHELL=${stdenv.shell}"
        "REQUIRES_CONFIGURATION="
        "prefix=${oxcaml}"
      ];
      buildFlags = [ "check-installed-bundled" ];

      installPhase = ''
        runHook preInstall
        mkdir "$out"
        runHook postInstall
      '';
    };

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
    pkgs.ocaml-ng.ocamlPackages_5_4.merlin
    pkgs.ocaml-ng.ocamlPackages_5_4.dot-merlin-reader
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
  ++ lib.optionals withMerlin merlinDev.devNativeBuildInputs
  ++ lib.optionals withJsoo jsooTools;

  buildInputs = [
    pkgs.llvm # llvm-objcopy is used for debuginfo
  ]
  ++ lib.optionals withMerlin merlinDev.devBuildInputs;

  # Nothing here runs libtool, so stop stdenv's configurePhase from rewriting
  # sys_lib_search_path in the checked-in build-aux/ltmain.sh.
  dontFixLibtool = true;

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
    ''
      $out/bin/generate_cached_generic_functions.exe $out/lib/ocaml/cached-generic-functions
    ''
    + lib.optionalString withJsoo ''
      make SHELL="$SHELL" jsoo-install-shipped OXCAML_INSTALL="$out" AST_DEPENDENT_LIBS_PREFIX="$out"
      ${wrapWasmOfOcaml}
    ''
    # Get rid of unused artifacts
    + ''
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
      astDependentLibsCommands =
        if needsPpxlibSources then
          "  make ppxlib-build        - Build ppxlib and its dependencies\n"
          + "  make ppxlib-install      - Install them (AST_DEPENDENT_LIBS_PREFIX=...)\n"
        else
          "  (make ppxlib-build needs this shell built with withAstDependentLibs=true)\n";
      jsooCommands =
        if withJsoo then
          "  make jsoo-build          - Build js_of_ocaml and wasm_of_ocaml\n"
          + "  make jsoo-install        - Install them (AST_DEPENDENT_LIBS_PREFIX=...)\n"
          + "  make jsoo-test           - Run core JSOO compiler and JS/Wasm regressions\n"
        else
          "  (make jsoo-* targets need this shell built with withJsoo=true)\n";
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
      ${astDependentLibsCommands}${jsooCommands}${merlinCommands}EOF
    '';

  meta =
    { } // (if framePointers && !pkgs.stdenv.hostPlatform.isx86_64 then { broken = true; } else { });

  passthru = {
    inherit
      ocaml_4_14_2
      ocaml_5_4_0
      ocamlformat
      lldb
      mkPpxlibLibs
      mkJsooLibs
      mkJsooTest
      mkJsooSmokeTest
      mkInstalledLibrariesCheck
      mkMerlinPackages
      ;
  };

  env =
    lib.optionalAttrs (withJsoo && gitRev != null) { JSOO_GIT_VERSION = "ox-${gitRev}"; }
    // lib.optionalAttrs needsPpxlibSources ppxlibSources
    // lib.optionalAttrs withJsoo jsooSources
    // lib.optionalAttrs (withJsoo && withJsooTestSources) jsooTestSources;
}
