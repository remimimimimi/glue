# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

# For information about flakes:
# https://serokell.io/blog/practical-nix-flakes

{
  description = "My haskell application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = "bozon";
      in {
        packages.${packageName} = haskellPackages.callCabal2nix packageName self
          rec {
            # FIXME: dependent-hashmap =
            #   jailbreakUnbreak haskellPackages.dependent-hashmap;
            # rock = jailbreakUnbreak haskellPackages.rock;
          };

        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server # you must build it with your ghc to work
            ghcid
            cabal-install
          ];
          # FIXME: Fix dependent-hashmap to fix this
          # inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
