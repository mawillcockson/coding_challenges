# from: https://blakesmith.me/2024/03/02/running-nixos-tests-with-flakes.html
{
  description = "NixOS tests example";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs @ {
    nixpkgs,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = [
        "x86_64-linux"
      ];
      perSystem = {
        config,
        self',
        inputs',
        pkgs,
        system,
        ...
      }: {
        packages = {
          helloNixosTests = pkgs.writeShellApplication {
            name = "hello-nixos-tests";
            runtimeInputs = [pkgs.netcat];
            text = ''
              nc -l 3000
            '';
          };
        };
      };
      flake = {};
    };
}
