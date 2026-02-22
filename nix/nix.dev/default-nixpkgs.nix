let
  nixpkgs = builtins.findFile [
    {
      prefix = "nixpkgs";
      path = import ./default-nixpkgs-url.nix;
    }
  ] "nixpkgs";
# use currently "recommended" way to import nixpkgs
# https://github.com/NixOS/nixpkgs/issues/339635#issue-2506369834
  pkgs = import "${nixpkgs}/pkgs/top-level" {
    config = {};
    overlays = [];
    localSystem.system = builtins.currentSystem;
  };
in {
  inherit pkgs nixpkgs;
}
