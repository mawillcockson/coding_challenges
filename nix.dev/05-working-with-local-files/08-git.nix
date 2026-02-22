{
  pkgs ? (import ./default-nixpkgs.nix { }).pkgs,
  config ? {
    shell = false;
  },
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
        mkdir -p "$out"

        pushd "$out"
        mkdir -p ./src/
        git init --initial-branch=main
        touch build.sh src/select.{c,h,o} README.md
        echo "hello" > hello.txt
        echo "world" > world.txt
        git add \
          hello.txt \
          world.txt \
          build.sh \
          README.md \
          src/select.{c,h,o}
        git config --local user.name "example"
        git config --local user.email "author@example.invalid"
        git commit -m "initial commit"
        popd
      '';
  shell = pkgs.lib.mkIf config.shell (
    pkgs.mkShellNoCC {
      name = "git-root";
      shellHook = "cd ${gitRoot}";
    }
  );
  gitTracked =
    pkgs.runCommandWith
      {
        name = "gitTracked";
        runLocal = true;
        stdenv = pkgs.stdenvNoCC;
        derivationArgs.nativeBuildInputs = [
          pkgs.nix
          pkgs.coreutils
        ];
      }
      ''
        GIT_ROOT="$TMPDIR/git"
        mkdir -p "$GIT_ROOT"
        cp -vR --no-preserve=ownership "${gitRoot}/." "$GIT_ROOT"
        NIX_STATE_DIR="$out/nix-state"
        export NIX_STATE_DIR
        NIX_STORE_DIR="$out/nix-store"
        export NIX_STORE_DIR
        XDG_CACHE_HOME="$out/nix-cache"
        export XDG_CACHE_HOME
        mkdir -p "$NIX_STATE_DIR" "$NIX_STORE_DIR" "$XDG_CACHE_HOME"
        ls -Alh "$GIT_ROOT/"
        nix-instantiate \
          --arg gitRoot "$GIT_ROOT" \
          --eval \
          --expr '{gitRoot}:
          let
            pkgs = import ${nixpkgs} { };
            fs = pkgs.lib.fileset;

          in
            fs.trace (fs.intersection
              (fs.gitTracked gitRoot)
              (fs.unions [
                (gitRoot + "/hello.txt")
                (gitRoot + "/world.txt")
                (gitRoot + "/build.sh")
                (gitRoot + "/src")
              ])
            ) ""
          ' 2>&1 | tee "$out/out.txt"
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
gitTracked
