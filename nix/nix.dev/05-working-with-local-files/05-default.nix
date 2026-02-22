{
  pkgs ? (import ./default-nixpkgs.nix { }).pkgs,
}:
pkgs.callPackage ./05-build.nix { }
