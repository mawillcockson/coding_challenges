{ pkgs ? (import (fetchTarball (import ../../default-nixpkgs-url.nix))) {
  config = { };
  overlays = [ ];
} }:
pkgs.mkShellNoCC {
  packages = [ ];
  shellHook = ''echo "hello from nix!"'';
}
