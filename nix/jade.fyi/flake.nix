{
  description = "https://jade.fyi/blog/flakes-arent-real/";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = {
    self,
    nixpkgs,
  }: {
    packages.x86_64-linux.hello-wrapper =
      nixpkgs.legacyPackages.x86_64-linux.callPackage ./hello-wrapper.nix
      {};
    packages.x86_64-linux.hello-wrapper-traditional =
      self.packages.x86_64-linux.hello-wrapper.override
      {
        traditionalGreeting = true;
      };
    packages.x86_64-linux.hello-runner =
      nixpkgs.legacyPackages.x86_64-linux.callPackage ./hello-runner
      {};
  };
}
