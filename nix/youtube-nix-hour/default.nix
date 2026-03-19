{system ? builtins.currentSystem}: let
  flake.lock = builtins.readFile ./flake.lock |> builtins.fromJSON;
  nixpkgs = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${flake.lock.nodes.nixpkgs.original.ref}.tar.gz?rev=${flake.lock.nodes.nixpkgs.locked.rev}";
    sha256 = (
      builtins.convertHash {
        hash = flake.lock.nodes.nixpkgs.locked.narHash;
        toHashFormat = "nix32";
      }
    );
  };
  pkgs = import "${nixpkgs}/pkgs/top-level" {
    config = {};
    overlays = [];
    localSystem = system;
  };
in
  pkgs.callPackage (
    {hello, ...}: {
      inherit hello;
    }
  ) {}
