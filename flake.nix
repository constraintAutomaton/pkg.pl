{
  description = "An experimental package manager for Scryer Prolog";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells = {
          default = pkgs.mkShell {
            buildInputs = builtins.attrValues {
              inherit (pkgs)
                git
                just
                util-linux
                shellcheck
                scryer-prolog
                ;
            };
          };
        };
      }
    );
}
