let
  /**
  * this is one way to specify a (somewhat) specific version of nixpkgs
  nixpkgs = builtins.findFile [{
    prefix = "nixpkgs";
    path = "channel:nixos-25.11-small";
  }] "nixpkgs";
  */
  # this is an equivalent way
  # nixpkgs = fetchTarball "channel:nixos-25.11";
  nixpkgs = fetchTarball (import ./default-nixpkgs-url.nix);
  pkgs = import nixpkgs {
    config = {};
    overlays = [];
  };
  lib = pkgs.lib;
  fs = lib.fileset;
  dir = fs.traceVal ./.;
in
  builtins.deepSeq dir dir
