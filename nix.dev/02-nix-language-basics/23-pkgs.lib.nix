let
  nixpkgs = fetchTarball (import ../../default-nixpkgs-url.nix);
  pkgs = import nixpkgs {
    config = { };
    overlays = [ ];
  };
in pkgs.lib.strings.toUpper "oh no, my caps lock key is stuck!"
