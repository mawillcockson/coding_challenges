#!/usr/bin/env nix-shell
#! nix-shell -i dash --pure
#! nix-shell --packages 'nix' 'dash'
#! nix-shell -I nixpkgs=channel:nixos-25.11-small
nix eval --extra-experimental-features nix-command --impure --expr '[<nixpkgs> <nixpkgs/lib>]'
