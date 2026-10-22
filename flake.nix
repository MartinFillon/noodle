{
  description = "Noodle - Haskell library for JSON, YAML, and TOML serialization and parsing";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        hPkgs = pkgs.haskell.packages.ghc910;

        # Clean source to avoid rebuilding on git/direnv/editor file changes
        src = pkgs.lib.cleanSource ./.;

        # Build noodle package with dependencies resolved by Nix
        # Also install the test runner binary into $out/bin so `nix run` works out of the box
        noodlePkg = pkgs.haskell.lib.compose.overrideCabal (drv: {
          doCheck = true;
          postInstall = (drv.postInstall or "") + ''
            install -D -m 755 dist/build/noodle-test/noodle-test $out/bin/noodle-test
            ln -sf noodle-test $out/bin/noodle
          '';
        }) (hPkgs.callCabal2nix "noodle" src { });

        # GHC environment containing noodle and its dependencies for interactive repl
        ghcEnv = hPkgs.ghcWithPackages (p: [ noodlePkg ]);

        replScript = pkgs.writeShellScriptBin "noodle-repl" ''
          exec ${ghcEnv}/bin/ghci -package noodle "$@"
        '';
      in
      {
        packages = {
          default = noodlePkg;
          noodle = noodlePkg;
        };

        apps = {
          default = {
            type = "app";
            program = "${noodlePkg}/bin/noodle";
            meta.description = "Run Noodle test suite";
          };
          test = {
            type = "app";
            program = "${noodlePkg}/bin/noodle-test";
            meta.description = "Run Noodle test suite";
          };
          repl = {
            type = "app";
            program = "${replScript}/bin/noodle-repl";
            meta.description = "Start a GHCi REPL with Noodle loaded";
          };
        };

        checks = {
          default = noodlePkg;
        };

        devShells.default = hPkgs.shellFor {
          packages = p: [ noodlePkg ];
          buildInputs = [
            hPkgs.cabal-install
            hPkgs.haskell-language-server
            pkgs.fourmolu
            pkgs.hlint
          ];
        };

        formatter = pkgs.nixfmt;
      }
    );
}
