{pkgs ? (import ../../default-nixpkgs-url.nix).pkgs}: let
  result = pkgs.lib.evalModules {
    modules = [
      ./00-basic-module.nix
      ./01-first-module.nix
    ];
  };
in
  result.config
