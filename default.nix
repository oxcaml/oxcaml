{
  nixpkgs ? import <nixpkgs> { },
  src ? ./.,
  addressSanitizer ? false,
  dev ? false,
  flambdaInvariants ? false,
  framePointers ? false,
  multidomain ? false,
  ocamltest ? true,
  pollInsertion ? false,
  runtime5 ? false,
  stackChecks ? false,
  warnError ? true,
  oxcamlClang ? false,
  oxcamlLldb ? false,
}:
let
  pkgs = nixpkgs.pkgs or nixpkgs;
  inherit (pkgs) lib fetchpatch;

  # Select stdenv based on whether asan is enabled
  myStdenv = if addressSanitizer then pkgs.clangStdenv else pkgs.stdenv;

  # Build configure flags based on features
  configureFlags =
    let
      mkFlag = bool: name: if bool then "--enable-${name}" else "--disable-${name}";
    in
    [
      "--cache-file=/dev/null"
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
    ];

  upstream = pkgs.ocaml-ng.ocamlPackages_4_14;

  ocaml = (upstream.ocaml.override { stdenv = myStdenv; }).overrideAttrs {
    # This patch is from oxcaml PR 3960, which fixes an issue in the upstream
    # compiler that we use to bootstrap ourselves on ARM64
    patches = [ ./arm64-issue-debug-upstream.patch ];
  };

  dune = upstream.dune_3.overrideAttrs (
    new: old: {
      version = "3.15.2";
      src = pkgs.fetchurl {
        url = "https://github.com/ocaml/dune/releases/download/${new.version}/dune-${new.version}.tbz";
        sha256 = "sha256-+VmYBULKhZCbPz+Om+ZcK4o3XzpOO9g8etegfy4HeTM=";
      };
    }
  );

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
myStdenv.mkDerivation {
  pname = "oxcaml";
  version = "5.2.0+ox";
  inherit src configureFlags;

  OXCAML_LLDB = if oxcamlLldb then "${lldb}/bin/lldb" else null;
  OXCAML_CLANG = if oxcamlClang then "${clang}/bin/clang" else null;

  enableParallelBuilding = true;

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
      upstream.ocamlformat_0_24_1 # required for make fmt
      pkgs.git
    ]
    ++ (if pkgs.stdenv.isDarwin then [ pkgs.cctools ] else [ pkgs.libtool ]) # cctools provides Apple libtool on macOS
    ++ lib.optional oxcamlLldb pkgs.python312;

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

  shellHook = ''
    export out="$(pwd)/_install"
    configureFlags+=" --prefix=$out"
    source ${pkgs.git}/share/bash-completion/completions/git-prompt.sh

    # Configure git ps1 options
    export GIT_PS1_SHOWDIRTYSTATE=1
    export GIT_PS1_SHOWSTASHSTATE=1
    export GIT_PS1_SHOWUNTRACKEDFILES=1
    export GIT_PS1_SHOWUPSTREAM="auto"

    # Set up prompt for both bash and zsh
    if [ -n "$BASH_VERSION" ]; then
      export PS1='[\u@\h \W$(__git_ps1 " (%s)")]\$ '
    elif [ -n "$ZSH_VERSION" ]; then
      setopt PROMPT_SUBST
      export PS1='[%n@%m %c$(__git_ps1 " (%s)")]\$ '
    fi

    cat >&2 << EOF
    $name Development Environment
    ==============================

    Configure flags: $configureFlags

    Available commands:
      configurePhase            - Delete build artifacts and re-run automake/configure
      make boot-compiler        - Quick build (recommended for development)
      make boot-_install        - Quick install (recommended for development)
      make fmt                  - Auto-format code
      make                      - Full build
      make install              - Install the compiler to ./_install
    EOF
  ''
  + lib.optionalString ocamltest ''
    cat >&2 << EOF
      make ci                   - Run all continuous integration tests
      make test                 - Run all tests
      make test-one TEST=...    - Run a single test
      make promote-one TEST=... - Accept the changed output of a failing test
    EOF
  ''
  ;
}
