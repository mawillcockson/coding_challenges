{
  nixpkgs ? (import ../../default-nixpkgs.nix).nixpkgs,
  pkgs ? (import ../../default-nixpkgs.nix).pkgs,
}: let
  fs = pkgs.lib.fileset;
  src = fs.toSource {
    root = ../../.;
    fileset = fs.unions [
      ../../default-nixpkgs-url.nix
      ./00-basic-module.nix
      (fs.fileFilter (
          {name, ...}:
            pkgs.lib.strings.hasPrefix "01-" name
        )
        ./.)
    ];
  };
in {
  helper =
    pkgs.runCommandWith
    {
      name = "helper";
      stdenv = pkgs.stdenvNoCC;
      runLocal = true;
      derivationArgs.nativeBuildInputs = [
        pkgs.nix
        pkgs.jq
      ];
    }
    ''
      options="$(set +o)"
      set -eu

      cp -vr '${src}/.' ./

      NIX_STATE_DIR="$TMPDIR/nix-state"
      export NIX_STATE_DIR
      NIX_STORE_DIR="$TMPDIR/nix-store"
      export NIX_STORE_DIR
      XDG_CACHE_HOME="$TMPDIR/nix-cache"
      export XDG_CACHE_HOME
      mkdir -pv "$NIX_STATE_DIR" "$NIX_STORE_DIR" "$XDG_CACHE_HOME"

      nix-instantiate \
        --json \
        --strict \
        --arg nixpkgs '${nixpkgs}' \
        --eval \
        ./07*/01*/01-default.nix \
          | jq -C . \
          | tee "$out"

      eval "$options"
    '';
}
