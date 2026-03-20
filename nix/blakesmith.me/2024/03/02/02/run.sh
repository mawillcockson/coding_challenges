#!/usr/bin/env nix-shell
#! nix-shell -i dash --pure
#! nix-shell -p dash nix
#! nix-shell -I nixpkgs=channel:nixos-25.11-small
set -eu
NIX_CONFIG='extra-experimental-features = nix-command flakes pipe-operators
allow-import-from-derivation = false'
export NIX_CONFIG

cleanup() {
    trap - EXIT QUIT TERM
}
trap cleanup EXIT QUIT TERM
set -x
nix flake check
