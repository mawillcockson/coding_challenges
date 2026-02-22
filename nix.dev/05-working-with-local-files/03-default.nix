{
  system ? builtins.currentSystem,
  nixpkgs ? (
    builtins.findFile [
      {
        prefix = "nixpkgs";
        path = (import ../default-nixpkgs-url.nix);
      }
    ] "nixpkgs"
  ),
}:
let
  pkgs = import nixpkgs {
    config = { };
    overlays = [ ];
    inherit system;
  };
in
pkgs.callPackage ./03-build.nix { }
