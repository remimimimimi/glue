{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        bozon = pkgs.rustPlatform.buildRustPackage rec {
          pname = "bozon";
          version = "0.1.0";
          src = self;
          cargoSha256 = "kasdf";
        };
      in {
        defaultPackage = bozon;
        packages = bozon;
        devShell = pkgs.mkShell { buildInputs = with pkgs; [ hello cowsay ]; };
      });
}
