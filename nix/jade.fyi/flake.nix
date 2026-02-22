{
  description = "https://jade.fyi/blog/flakes-arent-real/";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = {
    self,
    nixpkgs,
  }: {
    packages.x86_64-linux = nixpkgs.lib.makeScope nixpkgs.legacyPackages.x86_64-linux.newScope (self': {
      hello-wrapper = self'.callPackage ./hello-wrapper.nix {};
      hello-wrapper-traditional = self.hello-wrapper.override {
        traditionalGreeting = true;
      };
      hello-runner = self'.callPackage ./hello-runner.nix {};
    });
  };
}
