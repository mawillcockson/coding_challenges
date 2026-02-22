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
  fs = pkgs.lib.fileset;
in fs.trace ./. (pkgs.lib.runCommandWith {
  name = "ls";
  stdenv = pkgs.stdenvNoCC;
  runLocal = true;
  derivationArgs.nativeBuildInputs = [ pkgs.uutils-coreutils-noprefix ];
  buildCommand = ''
    exec "${pkgs.uutils-coreutils-noprefix}/bin/ls" -lAR
  '';
})
