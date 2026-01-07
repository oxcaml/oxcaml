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
  runtime5 ? false,
  stackChecks ? false,
  warnError ? true,
  oxcamlClang ? false,
  oxcamlLldb ? false,
  syntaxQuotations ? false,
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
      (mkFlag addressSanitizer "address-sanitizer")
      (mkFlag dev "dev")
      (mkFlag flambdaInvariants "flambda-invariants")
      (mkFlag framePointers "frame-pointers")
      (mkFlag multidomain "multidomain")
      (mkFlag pollInsertion "poll-insertion")
      (mkFlag runtime5 "runtime5")
      (mkFlag stackChecks "stack-checks")
      (mkFlag warnError "warn-error")
      (mkFlag ocamltest "ocamltest")
      (mkFlag syntaxQuotations "syntax-quotations")
    ];

  ocaml_5_4 = (pkgs.callPackage (
    import (pkgs.path + "/pkgs/development/compilers/ocaml/generic.nix") {
      major_version = "5";
      minor_version = "4";
      patch_version = "0";
      sha256 = "sha256-36qKLhHHmbwXZdi+9EkRQG7l9IAwJxkDgqk5+IyRImY=";
    }) {
      inherit stdenv;
    }).overrideAttrs {
      # This patch fixes an issue in the upstream compiler that we use to
      # bootstrap ourselves on ARM64
      patches = [
        ./tools/ci/local-opam/packages/ocaml-base-compiler/ocaml-base-compiler.5.4.0+oxcaml/files/ocaml-base-compiler.5.4.0+oxcaml.patch
      ];
    };

  ocamlPackages_5_4 =
    (pkgs.ocaml-ng.mkOcamlPackages ocaml_5_4).overrideScope (final: prev: {
      ppxlib =
        prev.ppxlib.overrideAttrs (old: rec {
          name = "${old.pname}-${version}";
          version = "0.37.0";
          src = pkgs.fetchurl {
            url = "https://github.com/ocaml-ppx/ppxlib/releases/download/${version}/ppxlib-${version}.tbz";
            sha256 = "sha256-LiI4N+fOzDvISkMkMsCnL04dW+kWXJwzdy8VbbhdsLM=";
          };
        });

      ppx_optcomp = prev.ppx_optcomp.overrideAttrs (old: rec {
        name = "${old.pname}-${version}";
        version = "0.17.1";
        src = pkgs.fetchFromGitHub {
          owner = "janestreet";
          repo = old.pname;
          rev = "v${version}";
          sha256 = "sha256-mi9YM0WGkc4sI1GF2YGTFwmPdF+4s5Ou2l7i07ys9nw=";
        };
      });

      ppxlib_jane = prev.ppxlib_jane.overrideAttrs (old: rec {
        name = "${old.pname}-${version}";
        version = "0.17.4";
        src = pkgs.fetchFromGitHub {
          owner = "janestreet";
          repo = old.pname;
          rev = "v${version}";
          sha256 = "sha256-cqF7aT0ubutRxsSTD5aHnHx4zvlPDkTzdBqONU6EgO0=";
        };
      });

      ppx_sexp_conv = prev.ppx_sexp_conv.overrideAttrs (old: rec {
        name = "${old.pname}-${version}";
        version = "0.17.1";
        src = pkgs.fetchFromGitHub {
          owner = "janestreet";
          repo = old.pname;
          rev = "v${version}";
          sha256 = "sha256-yQJluA/NSzCAID/ydBgRuc1sFHyjbXare9vxen6f1iw=";
        };
      });

      ppx_globalize = prev.ppx_globalize.overrideAttrs (old: rec {
        name = "${old.pname}-${version}";
        version = "0.17.2";
        src = pkgs.fetchFromGitHub {
          owner = "janestreet";
          repo = old.pname;
          rev = "v${version}";
          sha256 = "sha256-5pHqyv94DXpSG69TEATcnJwFh5YurxVCM5ZPtrlbXSo=";
        };
      });

      ppx_inline_test = prev.ppx_inline_test.overrideAttrs (old: rec {
        name = "${old.pname}-${version}";
        version = "0.17.2";
        src = pkgs.fetchFromGitHub {
          owner = "janestreet";
          repo = old.pname;
          rev = "v${version}";
          sha256 = "sha256-wNDDdNUeWTW87HRKbRSuOXaCPQnDWx7/RXuCDISc9Pg=";
        };
      });

      ppx_expect = prev.ppx_expect.overrideAttrs (old: rec {
        name = "${old.pname}-${version}";
        version = "0.17.3";
        src = pkgs.fetchFromGitHub {
          owner = "janestreet";
          repo = old.pname;
          rev = "v${version}";
          sha256 = "sha256-eYZ3p3FYjHd15pj79TKyHSHNKRSWj80iHJFxBZN40s4=";
        };
      });

      ocamlformat_0_28_1 =
        prev.ocamlformat.overrideAttrs (old: rec {
          name = "${old.pname}-${version}";
          version = "0.28.1";
          src = pkgs.fetchurl {
            url = "https://github.com/ocaml-ppx/ocamlformat/releases/download/${version}/ocamlformat-${version}.tbz";
            sha256 = "sha256-cL2gN9C+2WHtkb21GYsu7vVCREdQqLAV2AzLlLP/Qfs=";
          };
        });
      ocamlformat = final.ocamlformat_0_28_1;
    });

  upstream = pkgs.ocaml-ng.ocamlPackages_4_14;
  ocaml = (upstream.ocaml.override { inherit stdenv; }).overrideAttrs {
    # This patch is from oxcaml PR 3960, which fixes an issue in the upstream
    # compiler that we use to bootstrap ourselves on ARM64
    patches = [
      ./tools/ci/local-opam/packages/ocaml-base-compiler/ocaml-base-compiler.4.14.2+oxcaml/files/ocaml-base-compiler.4.14.2+oxcaml.patch
    ];
  };

  dune = upstream.dune_3.overrideAttrs rec {
    version = "3.19.1";
    src = pkgs.fetchurl {
      url = "https://github.com/ocaml/dune/releases/download/${version}/dune-${version}.tbz";
      hash = "sha256-oQOG+YDNqUF9FGVGa+1Q3SrvnJO50GoPf+7tsKFUEVg=";
    };
  };

  menhirLib = upstream.menhirLib.overrideAttrs (
    new: old: rec {
      version = "20231231";
      src = pkgs.fetchFromGitLab {
        domain = "gitlab.inria.fr";
        owner = "fpottier";
        repo = "menhir";
        rev = version;
        sha256 = "sha256-veB0ORHp6jdRwCyDDAfc7a7ov8sOeHUmiELdOFf/QYk=";
      };
    }
  );

  menhir =
    let
      menhirSdk = upstream.menhirSdk.override { inherit menhirLib; };
    in
    (upstream.menhir.override { inherit menhirLib; }).overrideAttrs (
      new: old: {
        buildInputs = [
          menhirLib
          menhirSdk
        ];
        postInstall = ''
          ln -s ${menhirLib}/lib/ocaml/*/site-lib/menhirLib $out/lib/
        '';
      }
    );

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
        swig
      ];

      cmakeFlags = [
        "-DLLVM_ENABLE_PROJECTS=${lib.strings.concatStringsSep ";" projects}"
        "-DCMAKE_BUILD_TYPE=Release"
        "-DLLVM_TARGETS_TO_BUILD=Native"
        "-DLLDB_ENABLE_PYTHON=ON"
        "-DLLDB_ENABLE_LIBEDIT=ON"
        "-DLLDB_ENABLE_CURSES=ON"
        # Disable tests to avoid needing libc++
        "-DLLDB_INCLUDE_TESTS=OFF"
        "-DLLVM_INCLUDE_TESTS=OFF"
        "-DCLANG_INCLUDE_TESTS=OFF"
      ];

      sourceRoot = "source/llvm";
      enableParallelBuilding = true;
    }

  ;

  lldb = makeLlvm {
    pname = "oxcaml-lldb";
    version = "16.0.6-minus0";
    projects = [
      "clang"
      "lldb"
    ];
    src = pkgs.fetchFromGitHub {
      owner = "ocaml-flambda";
      repo = "llvm-project";
      tag = "oxcaml-lldb-16.0.6-minus0";
      sha256 = "sha256-ZIbcC1wj2U9QYt3s1kOYPs+gtaCX+EXfMC3WiiF821E=";
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
  version = "5.2.0+ox";
  inherit src configureFlags;

  OXCAML_LLDB = if oxcamlLldb then "${lldb}/bin/lldb" else null;
  OXCAML_CLANG = if oxcamlClang then "${clang}/bin/clang" else null;

  enableParallelBuilding = true;
  separateDebugInfo = !dev;

  # Disable _multioutConfig hook which adds --libdir=$out/lib into
  # configureFlags when separateDebugInfo is enabled, breaking OCaml's configure
  # step, which expects --libdir to be $out/lib/ocaml
  setOutputFlags = false;

  nativeBuildInputs =
    [
      pkgs.autoconf
      menhir
      ocaml
      dune
      pkgs.pkg-config
      pkgs.rsync
      pkgs.which
      pkgs.parallel
      gfortran # Required for Bigarray Fortran tests
      upstream.ocamlformat_0_28_1 # required for make fmt
      pkgs.removeReferencesTo
    ]
    ++ (if pkgs.stdenv.isDarwin then [ pkgs.cctools ] else [ pkgs.libtool ]) # cctools provides Apple libtool on macOS
    ++ lib.optional oxcamlLldb pkgs.python312;

  buildInputs = [
    pkgs.llvm # llvm-objcopy is used for debuginfo
  ];

  preConfigure = ''
    rm -rf _build _install _runtest

    # We don't use autoreconfHook because libtoolize and autoheader are
    # incompatible with ocaml-flambda
    autoconf --force
  '';

  checkPhase = lib.optionalString ocamltest ''
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

  shellHook = ''
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
    EOF
  '';

  meta =
    { } // (if framePointers && !pkgs.stdenv.hostPlatform.isx86_64 then { broken = true; } else { });

  passthru = {
    inherit ocaml_5_4 ocamlPackages_5_4;
  };
}
