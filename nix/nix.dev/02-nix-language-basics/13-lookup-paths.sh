#!/usr/bin/env nix-shell
#! nix-shell -i dash --pure
#! nix-shell --packages 'nix' 'dash'
#! nix-shell -I nixpkgs=channel:nixos-25.11-small
NIX_PATH="$(nix-instantiate --eval --strict --raw ../../default-nixpkgs-url.nix)"
export NIX_PATH
nix eval --extra-experimental-features nix-command --impure --expr '[<nixpkgs> <nixpkgs/lib>]'
