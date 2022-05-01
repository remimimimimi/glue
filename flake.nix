{
  description = "My cute Rust crate!";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    naersk.url = "github:nmattia/naersk";
    naersk.inputs.nixpkgs.follows = "nixpkgs";
    utils.url = "github:numtide/flake-utils";
    mozillapkgs = {
      url = "github:mozilla/nixpkgs-mozilla";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, utils, naersk, mozillapkgs }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}";
        mozilla = pkgs.callPackage (mozillapkgs + "/package-set.nix") { };
        rust = (mozilla.rustChannelOf {
          rustToolchain = ./rust-toolchain;
          sha256 = "AtA8M47G6aCq/VqFywceRWX28BLhKEuXrg7YAZ3PUHs=";
        }).rust;
        naersk-lib = naersk.lib."${system}".override {
          cargo = rust;
          rustc = rust;
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
        apps.bozon = utils.lib.mkApp { drv = packages.bozon; };
        defaultApp = apps.bozon;

        # `nix develop`
        devShell = pkgs.mkShell {
          nativeBuildInputs = [ rust pkgs.llvmPackages_13.libllvm ];
          LIBCLANG_PATH = "${pkgs.llvmPackages_13.libclang.lib}/lib";
        };
      });
}
