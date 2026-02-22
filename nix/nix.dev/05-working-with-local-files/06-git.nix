{
  pkgs ? (import ./default-nixpkgs.nix { }).pkgs,
}:
let
  fs = pkgs.lib.fileset;
  nixpkgs = (import ./default-nixpkgs.nix { }).nixpkgs;
  gitRoot =
    pkgs.runCommandWith
      {
        name = "git-root";
        stdenv = pkgs.stdenvNoCC;
        runLocal = true;
        derivationArgs.nativeBuildInputs = [
          pkgs.git
          pkgs.nix
        ];
      }
      ''
        set -eu
        mkdir -p "$out/git"

        pushd "$out/git"
        mkdir -p ./src/
        git init --initial-branch=main
        touch build.sh src/select.{c,h,o} README.md
        echo "hello" > hello.txt
        echo "world" > world.txt
        git add build.sh README.md src/select.{c,h,o}
        git config --local user.name "example"
        git config --local user.email "author@example.invalid"
        git commit -m "initial commit"
        popd

        NIX_STATE_DIR="$out/nix-state"
        export NIX_STATE_DIR
        NIX_STORE_DIR="$out/nix-store"
        export NIX_STORE_DIR
        XDG_CACHE_HOME="$out/nix-cache"
        export XDG_CACHE_HOME
        mkdir -p "$NIX_STATE_DIR" "$NIX_STORE_DIR" "$XDG_CACHE_HOME"
        nix-instantiate \
          --arg gitRoot "$out/git" \
          --eval \
          --expr '{gitRoot}:
          let
            pkgs = import ${nixpkgs} { };
            fs = pkgs.lib.fileset;

          in
            fs.trace (fs.gitTracked gitRoot) ""
          '
      '';
  print = (x: builtins.trace x x);
  ls =
    pkgs.runCommandWith
      {
        name = "ls";
        stdenv = pkgs.stdenvNoCC;
        runLocal = true;
        derivationArgs.nativeBuildInputs = [ pkgs.coreutils ];
      }
      ''
        ls -ARlh "${gitRoot}" | tee "$out"
      '';

in
print gitRoot
