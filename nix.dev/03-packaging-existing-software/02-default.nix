let
  nixpkgs = builtins.findFile [{
    prefix = "nixpkgs";
    path = (import ../default-nixpkgs-url.nix);
  }] "nixpkgs";
  pkgs = import nixpkgs {
    config = { };
    overlays = [ ];
  };
in {
  hello = pkgs.callPackage ./01-hello.nix { };
  icat = pkgs.callPackage ./02-icat.nix { };
}
