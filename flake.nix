{
  description = "Noodle";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.systems.url = "github:nix-systems/default";
  inputs.flake-utils = {
    url = "github:numtide/flake-utils";
    inputs.systems.follows = "systems";
  };

  outputs =
    { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        hPkgs = pkgs.haskell.packages.ghc910;

        defaultPackages = [
          hPkgs.stack
          hPkgs.ghc
        ];
      in
      {
        devShells.default = pkgs.mkShell {
          packages =
            defaultPackages
            ++ (with pkgs; [
              cabal2nix
            ]);

          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath defaultPackages;
        };

        packages.default = hPkgs.callCabal2nix "noodle" ./. { };
      }
    );
}
