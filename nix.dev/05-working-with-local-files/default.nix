{ system ? builtins.currentSystem, nixpkgs ? builtins.findFile [{
  prefix = "nixpkgs";
  path = "channel:nixos-25.11-small";
}] "nixpkgs" }:
let
  pkgs = import nixkpgs {
    config = { };
    overlays = [ ];
    inherit system;
  };
in pkgs.callPackage ./build.nix { }
