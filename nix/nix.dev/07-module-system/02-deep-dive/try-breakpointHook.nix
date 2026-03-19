{pkgs ? (import ../../default-nixpkgs.nix).pkgs, ...}:
pkgs.hello.overrideDerivation (_: {
  nativeBuildInputs = [pkgs.breakpointHook];
  prePatch = "breakpointHook";
})
