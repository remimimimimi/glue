{
  description = "My cute Rust crate!";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    naersk = {
      url = "github:nmattia/naersk";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, naersk, fenix, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        rust-toolchain = fenix.packages.${system}.fromToolchainFile {
          file = ./rust-toolchain.toml;
          sha256 = "otgm+7nEl94JG/B+TYhWseZsHV1voGcBsW/lOD2/68g=";
        };
        naersk-lib = naersk.lib.${system}.override {
          cargo = rust-toolchain;
          rustc = rust-toolchain;
        };
      in rec {
        # `nix build`
        packages = {
          bozon = naersk-lib.buildPackage {
            pname = "bozon";
            root = ./.;
            buildInputs = [ pkgs.llvmPackages_13.libllvm ];
          };
        };

        defaultPackage = packages.bozon;

        # `nix run`
        apps.bozon = flake-utils.lib.mkApp { drv = packages.bozon; };
        defaultApp = apps.bozon;

        # `nix develop`
        devShell = pkgs.mkShell {
          nativeBuildInputs = [ rust-toolchain pkgs.llvmPackages_13.libllvm ];
          LIBCLANG_PATH = "${pkgs.llvmPackages_13.libclang.lib}/lib";
        };
      });
}
