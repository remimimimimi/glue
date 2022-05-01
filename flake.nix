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
  # overlay = { };
  # packages = { };
  # defaultPackage = { };
  # check = { };
  # devShell = { };
  # let
  #   cargoToml = (builtins.fromTOML (builtins.readFile ./Cargo.toml));
  #   supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" ];
  #   forAllSystems = f:
  #     nixpkgs.lib.genAttrs supportedSystems (system: f system);
  # in {
  #   overlay = final: prev: {
  #     "${cargoToml.package.name}" = final.callPackage ./. { inherit naersk; };
  #   };

  #   packages = forAllSystems (system:
  #     let
  #       pkgs = import nixpkgs {
  #         inherit system;
  #         overlays = [ self.overlay ];
  #       };
  #     in { "${cargoToml.package.name}" = pkgs."${cargoToml.package.name}"; });

  #   defaultPackage = forAllSystems (system:
  #     (import nixpkgs {
  #       inherit system;
  #       overlays = [ self.overlay ];
  #     })."${cargoToml.package.name}");

  #   checks = forAllSystems (system:
  #     let
  #       pkgs = import nixpkgs {
  #         inherit system;
  #         overlays = [ self.overlay ];
  #       };
  #     in {
  #       format = pkgs.runCommand "check-format" {
  #         buildInputs = with pkgs; [ rustfmt cargo ];
  #       } ''
  #         ${pkgs.rustfmt}/bin/cargo-fmt fmt --manifest-path ${
  #           ./.
  #         }/Cargo.toml -- --check
  #         ${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt --check ${./.}
  #         touch $out # it worked!
  #       '';
  #       "${cargoToml.package.name}" = pkgs."${cargoToml.package.name}";
  #     });

  #   devShell = forAllSystems (system:
  #     let
  #       pkgs = import nixpkgs {
  #         inherit system;
  #         overlays = [ self.overlay ];
  #       };
  #     in pkgs.mkShell {
  #       inputsFrom = with pkgs; [ pkgs."${cargoToml.package.name}" ];
  #       buildInputs = with pkgs; [ rustfmt nixpkgs-fmt ];
  #       LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
  #     });
  # };
}
