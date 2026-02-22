{
  nixpkgs ? (
    builtins.findFile [
      {
        prefix = "nixpkgs";
        path = (import ../default-nixpkgs-url.nix);
      }
    ] "nixpkgs"
  ),
  system ? builtins.currentSystem,
}:
let
  pkgs = import nixpkgs {
    config = { };
    overlays = [ ];
    inherit system;
  };
  targetSystem = pkgs.pkgsCross.aarch64-multiplatform.stdenv.hostPlatform;
  pkgsCross = import nixpkgs {
    crossSystem = {
      inherit (targetSystem) config;
    };
    config = { };
    overlays = [ ];
    inherit system;
  };
in
pkgsCross.hello
