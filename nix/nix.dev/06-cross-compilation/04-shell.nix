let
  nixpkgs = (
    builtins.findFile [
      {
        prefix = "nixpkgs";
        path = (import ../default-nixpkgs-url.nix);
      }
    ] "nixpkgs"
  );
  pkgs = import nixpkgs {
    config = { };
    overlays = [ ];
    inherit (builtins) currentSystem;
  };
  inherit (pkgs) pkgsStatic;
  helloWorldC = pkgs.writeText "hello.c" ''
    #include <stdio.h>

    int main (void) {
      printf ("Hello, world!\n");
      return 0;
    }
  '';

in
pkgsStatic.runCommandWith
  {
    name = "compile-and-check";
    derivationArgs = {
      nativeBuildInputs = [
        pkgsStatic.pkg-config
        pkgsStatic.file
      ];
      buildInputs = [ pkgsStatic.zlib ];
    };
    runLocal = true;
  }
  ''
    options="$(set +o)"
    set -eu

    mkdir -pv "$out"

    "$CC" '${helloWorldC}' -o "$out/hello"
    file "$out/hello" | tee "$out/description.txt"

    eval "$options"
  ''
