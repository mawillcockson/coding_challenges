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
in
pkgs.pkgsCross.aarch64-multiplatform.hello
