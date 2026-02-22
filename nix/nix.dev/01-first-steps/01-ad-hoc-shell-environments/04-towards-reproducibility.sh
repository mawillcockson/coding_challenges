#!/usr/bin/env nix-shell
#! nix-shell -i dash --pure
#! nix-shell -p dash git
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/tarball/2a601aafdc5605a5133a2ca506a34a3a73377247
git --version
