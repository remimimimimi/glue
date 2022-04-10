{
  inputs = { nixpkgs.url = "github:nixos/nixpkgs"; };

  output = { self, nixpkgs }: {
    packages.x86_64-linux.hello = nixpkgs.legacyPackages.x86_64-linux.hello;
  };
}
