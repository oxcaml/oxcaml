{
  description = "OxCaml - A performance-focused fork of OCaml";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/718f670e845328e9c2cecdd832835712b2cbd065";
    flake-utils.url = "github:numtide/flake-utils";
    nix-github-actions = {
      url = "github:nix-community/nix-github-actions";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      nix-github-actions,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        lib = pkgs.lib;
        oxcaml = pkgs.callPackage ./default.nix {
          src = self;
          gitRev = self.shortRev or self.dirtyShortRev or null;
        };
        merlinPackages = oxcaml.mkMerlinPackages oxcaml;
      in
      {
        packages = {
          inherit oxcaml;
          inherit (merlinPackages) merlin-lib dot-merlin-reader merlin;
          oxcaml-fp = oxcaml.override { framePointers = true; };
          oxcaml-asan = oxcaml.override { addressSanitizer = true; };
          ppxlib = oxcaml.mkPpxlibLibs oxcaml;
          jsoo = oxcaml.mkJsooLibs oxcaml;
          jsoo-smoke-test = oxcaml.mkJsooSmokeTest oxcaml;
          installed-libraries = oxcaml.mkInstalledLibrariesCheck oxcaml;
          default = oxcaml;
        };

        checks = lib.attrsets.filterAttrs (key: drv: !(drv.meta.broken or false)) {
          inherit (self.packages.${system})
            oxcaml
            oxcaml-fp
            oxcaml-asan
            ppxlib
            jsoo
            jsoo-smoke-test
            installed-libraries
            merlin
            ;
        };

        formatter = pkgs.nixfmt-tree;

        # Use the compiler derivation itself as the dev shell so `nix develop`
        # exposes its full build environment (configureFlags, preConfigure,
        # OXCAML_LLDB/OXCAML_CLANG, ...) and the `configurePhase` advertised by
        # the shellHook behaves exactly like the nix build. withMerlin only
        # extends its inputs with what `make merlin-build` / `make merlin-test`
        # need.
        devShells.default = oxcaml.override {
          withMerlin = true;
          withJsooTestSources = true;
        };
      }
    )
    // {
      githubActions = nix-github-actions.lib.mkGithubMatrix {
        checks = nixpkgs.lib.getAttrs [
          "x86_64-linux"
          "aarch64-linux"
          "aarch64-darwin"
        ] self.checks;
        platforms = {
          "x86_64-linux" = "warp-ubuntu-latest-x64-16x";
          "aarch64-linux" = "warp-ubuntu-latest-arm64-16x";
          "aarch64-darwin" = "warp-macos-15-arm64-12x";
        };
      };

      # What .github/workflows/oxfuzzer.yml fuzzes.
      oxfuzzerGithubActions =
        let
          # The part of githubActions' matrix for these compiler variants, so
          # every row is a check the trusted Nix workflow built (and pushed to
          # the cache) for the commit being fuzzed: a variant that is broken on
          # a system is missing from self.checks and drops out of both. Each
          # row is the very derivation the trusted workflow built, so fuzzing
          # only downloads it.
          all = nix-github-actions.lib.mkGithubMatrix {
            checks = builtins.mapAttrs (
              _system:
              nixpkgs.lib.filterAttrs (
                name: _drv:
                builtins.elem name [
                  "oxcaml"
                  "oxcaml-fp"
                  "oxcaml-asan"
                ]
              )
            ) self.githubActions.checks;
            # Fuzzing is CPU-bound, so use bigger runners than githubActions.
            platforms = {
              "x86_64-linux" = "warp-ubuntu-latest-x64-32x";
              "aarch64-linux" = "warp-ubuntu-latest-arm64-32x";
              "aarch64-darwin" = "warp-macos-15-arm64-12x";
            };
          };

          # The matrix restricted to the rows whose whole variant and system
          # match these regular expressions (an empty one matches everything).
          # Fails if no row is left.
          select =
            {
              variants ? "",
              systems ? "",
            }:
            let
              matches =
                pattern: string:
                pattern == "" || builtins.match pattern string != null;
              include = builtins.filter (
                row: matches variants row.name && matches systems row.system
              ) all.matrix.include;
            in
            if include == [ ] then
              throw (
                "oxfuzzerGithubActions: no row matches variants '${variants}' "
                + "and systems '${systems}'"
              )
            else
              all.matrix // { inherit include; };
        in
        all
        // {
          inherit select;
          # `select` with the patterns in the environment variables
          # OXFUZZER_VARIANTS and OXFUZZER_SYSTEMS, for the workflow. Evaluate
          # it with --impure: otherwise it sees neither variable and selects
          # every row.
          selected = select {
            variants = builtins.getEnv "OXFUZZER_VARIANTS";
            systems = builtins.getEnv "OXFUZZER_SYSTEMS";
          };
        };
    };
}
