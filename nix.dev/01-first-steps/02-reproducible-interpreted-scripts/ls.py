#!/usr/bin/env nix-shell
#! nix-shell -i python --pure
#! nix-shell --packages 'python3Minimal'
#! nix-shell -I nixpkgs=channel:nixos-25.11-small
from pathlib import Path


def main() -> None:
    for path in Path(".").glob("**"):
        print(path)


if __name__ == "__main__":
    main()
