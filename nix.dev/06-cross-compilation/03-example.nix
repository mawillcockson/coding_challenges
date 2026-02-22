{
  nixpkgs ? (
    builtins.findFile [
      {
        prefix = "nixpkgs";
        path = (import ../default-nixpkgs-url.nix);
      }
    ] "nixpkgs"
  ),
  system ? builtins.currentSystem,
}:
let
  pkgs = import nixpkgs {
    config = { };
    overlays = [ ];
    inherit system;
  };
  helloWorldC = pkgs.writeText "hello.c" ''
    #include <stdio.h>

    int main (void) {
      printf ("Hello, world!\n");
      return 0;
    }
  '';
  cross-compile-and-emulate =
    hostPkgs:
    hostPkgs.runCommandCC "hello-world-cross-test" { } ''
      # WINE requires home directory
      HOME="$PWD"
      export HOME

      # Compile hello world using the compiler for our specific platform
      "$CC" '${helloWorldC}' -o ./hello

      # Run the compiled program using user mode emulation (Qemu/Wine)
      # buildPackages is passed so that emulation is built for the build platform
      '${hostPkgs.stdenv.hostPlatform.emulator hostPkgs.buildPackages}' ./hello | tee $out
    '';
in
{
  rpi = cross-compile-and-emulate pkgs.pkgsCross.raspberryPi;
  windows = cross-compile-and-emulate pkgs.pkgsCross.mingwW64;
}
