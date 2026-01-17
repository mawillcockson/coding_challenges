let
  nixpkgs = fetchTarball "channel:nixos-25.11";
  pkgs = import nixpkgs {
    config = {};
    overlays = [];
  };
in {
  hello = pkgs.callPackage ./hello.nix {};
  icat = pkgs.callPackage ./icat.nix {};
}
