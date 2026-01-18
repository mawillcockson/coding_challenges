let
  nixpkgs = builtins.findFile [{
    prefix = "nixpkgs";
    path = "channel:nixos-25.11-small";
  }] "nixpkgs";
  pkgs = import nixpkgs {
    config = { };
    overlays = [ ];
  };
  lib = pkgs.lib;
  fs = lib.fileset;
  dir = fs.traceVal ./.;
in builtins.deepSeq dir dir
