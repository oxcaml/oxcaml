{
  nixpkgs ? import <nixpkgs> { },
  src ? ./.,
  enablePollInsertion ? false,
  enableRuntime5 ? false,
  enableFramePointers ? false,
  enableAddressSanitizer ? false,
  enableMultidomain ? false,
  enableStackChecks ? false,
}:
let
  pkgs = nixpkgs.pkgs or pkgs;

  # Build configure flags based on features
  configureFlags =
    pkgs.lib.optionals enablePollInsertion [ "--enable-poll-insertion" ]
    ++ pkgs.lib.optionals enableRuntime5 [ "--enable-runtime5" ]
    ++ pkgs.lib.optionals enableFramePointers [ "--enable-frame-pointers" ]
    ++ pkgs.lib.optionals enableAddressSanitizer [ "--enable-address-sanitizer" ]
    ++ pkgs.lib.optionals enableMultidomain [ "--enable-multidomain" ]
    ++ pkgs.lib.optionals enableStackChecks [ "--enable-stack-checks" ];

  # Select stdenv based on whether asan is enabled
  myStdenv = if enableAddressSanitizer then pkgs.clangStdenv else pkgs.stdenv;

  upstream = pkgs.ocaml-ng.ocamlPackages_4_14;

  ocaml = (upstream.ocaml.override { stdenv = myStdenv; }).overrideAttrs ({
    configurePlatforms = [ ];

    # CR-soon jvanburen: for some reason, SOMETHING is putting nix stuff in our
    # path even though el8 says not to. We should fix that... For now, set it
    # explicitly
    preConfigure =
      let
        nonNixPaths = builtins.filter (
          path: !(pkgs.lib.hasPrefix "/nix" (toString path))
        ) myStdenv.initialPath;
      in
      if nonNixPaths != [ ] then
        ''
          export PATH="${pkgs.lib.strings.concatMapStringsSep ":" (x: "${x}/bin") nonNixPaths}:$PATH"
        ''
      else
        "";
  });

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

  ocamlformat = upstream.ocamlformat_0_24_1;

  # Banner for development environment
  banner =
    let
      enabledness = bool: if bool then "enabled" else "disabled";
    in
    ''
      OxCaml Development Environment
      ==============================

      Features:
        Address Sanitizer ${enabledness enableAddressSanitizer}
        Frame pointers ${enabledness enableFramePointers}
        Multidomain ${enabledness enableMultidomain}
        Poll insertion ${enabledness enablePollInsertion}
        Runtime 5 ${enabledness enableRuntime5}
        Stack checks ${enabledness enableStackChecks}

      Available commands:
        make boot-compiler       - Quick build (recommended for development)
        make boot-_instal        - Quick install (recommended for development)
        make fmt                 - Auto-format code
        make                     - Full build
        make install             - Install
        make test                - Run all tests
        make test-one TEST=...   - Run a single test
    '';

in
myStdenv.mkDerivation {
  pname = "oxcaml";
  version = "5.2.0+ox";
  inherit src configureFlags;

  enableParallelBuilding = true;

  nativeBuildInputs = [
    pkgs.autoconf
    pkgs.libtool
    menhir
    ocaml
    dune
    pkgs.pkg-config
    pkgs.rsync
    pkgs.which
  ];

  preConfigure = ''
    rm -rf _build
    # Ensure that we still use the same toolchain after we
    # are out of nix, rather than whatever is in the user's PATH.
    CC="${myStdenv.cc.meta.mainProgram}"

    if [[ $CC == gcc ]]; then
        # It seems like oxcaml doesn't pass the build prefix
        # flag when AS is not as. We should fix that
        AS=as
    else
        AS="$CC -c"
    fi

    export AS CC

    if [[ $- != *i* ]]; then
      # Avoid putting dune into the Nix closure by ensuring that the full Nix path
      # to dune doesn't appear in the output anywhere
      mkdir -p .local/bin
      ln -s ${dune}/bin/dune .local/bin/dune
      configureFlags+=" --cache-file=/dev/null --with-dune=$PWD/.local/bin/dune"
    fi

    # We don't use autoreconfHook because libtoolize and autoheader are
    # incompatible with ocaml-flambda
    autoconf --force
  '';

  postInstall =
    # Get rid of unused artifacts
    ''
      $out/bin/generate_cached_generic_functions.exe $out/lib/ocaml/cached-generic-functions
      (cd $out/bin
        rm -f dumpobj.byte
        rm -f extract_externals.byte
        rm -f generate_cached_generic_functions.exe
        rm -f ocamlcp
        rm -f ocamlmklib.byte
        rm -f ocamlmktop.byte
        rm -f ocamlobjinfo.byte
        rm -f ocamlopt.byte
        rm -f ocamlprof
      )
    '';

  passthru = {
    inherit configureFlags;

    devTools = [
      pkgs.gdb
      pkgs.perf-tools
      ocamlformat
      pkgs.git
    ];
  };

  shellHook = ''
    export configureFlags+="--enable-dev --enable-warn-error --enable-ocamltest --prefix=$(pwd)/_install"
    export PS1='$name$ '
    echo ${pkgs.lib.escapeShellArg banner}

  '';
}
