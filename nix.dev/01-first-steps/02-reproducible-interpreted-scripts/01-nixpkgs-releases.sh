#!/usr/bin/env nix-shell
#! nix-shell -i dash --pure
#! nix-shell -p dash cacert curl jq python3Packages.xmljson
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/2a601aafdc5605a5133a2ca506a34a3a73377247.tar.gz
set -eux
curl 'https://github.com/NixOS/nixpkgs/releases.atom' \
    | xml2json \
    | jq -C .
