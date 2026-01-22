#!/usr/bin/env nix-shell
#! nix-shell -i dash
#! nix-shell -p dash nix
#! nix-shell -I nixpkgs=channel:nixos-25.11-small
set -eu
if ! STORE_PATH="$(nix-instantiate --eval --strict --raw --expr '"${./24-paths.sh}"')"; then
    printf 'error with nix-instantiate!\n'
    exit 1
fi
set +e
set -x
ls -Alh "${STORE_PATH}"
