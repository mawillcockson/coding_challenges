#!/usr/bin/env sh
set -eux
nix eval -f 03-eval.nix \
    --apply 'x: x {
        pkgs =
            import ../../nix.dev/default-nixpkgs.nix
            |> builtins.getAttr "pkgs";
    }' \
    --json | jq -C .
