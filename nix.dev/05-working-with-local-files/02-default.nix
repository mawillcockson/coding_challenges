{ system ? builtins.currentSystem, nixpkgs ? builtins.findFile [{
  prefix = "nixpkgs";
  path = (import ../../default-nixpkgs-url.nix);
}] "nixpkgs" }:
let
  pkgs = import nixkpgs {
    config = { };
    overlays = [ ];
    inherit system;
  };
in pkgs.callPackage ./build.nix { }
