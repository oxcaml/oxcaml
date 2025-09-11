{
  description = "OxCaml - A performance-focused fork of OCaml";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        packages = {
          oxcaml = pkgs.callPackage ./default.nix { };
          default = self.packages.${system}.oxcaml;
        };

        formatter = pkgs.nixfmt-tree;

        devShells.default = self.packages.${system}.oxcaml;
      }
    );
}
