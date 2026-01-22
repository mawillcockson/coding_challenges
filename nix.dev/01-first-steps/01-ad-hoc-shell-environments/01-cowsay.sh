#!/usr/bin/env nix-shell
#! nix-shell -i dash --pure
#! nix-shell -p dash nix
#! nix-shell -I nixpkgs=channel:nixos-25.11-small
set -eux
nix-shell \
    -I nixpkgs="channel:nixos-25.11-small" \
    -p cowsay \
    --pure \
    --run 'cowsay -e "^^" "Nix + Nixpkgs"'
