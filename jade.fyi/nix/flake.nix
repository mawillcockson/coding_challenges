{
  description = "https://jade.fyi/blog/flakes-arent-real/";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = {
    self,
    nixpkgs,
  }: {
    packages.x86_64-linux.hello-runner =
      nixpkgs.legacyPackages.x86_64-linux.callPackage ./package.nix
      {};
    packages.x86_64-linux.hello-runner-traditional = self.packages.x86_64-linux.hello-runner.override {
      traditionalGreeting = true;
    };
  };
}
