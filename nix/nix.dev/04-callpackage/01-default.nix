let
  nixpkgs = builtins.findFile [{
    prefix = "nixpkgs";
    path = (import ../default-nixpkgs-url.nix);
  }] "nixpkgs";
  pkgs = import nixpkgs {
    config = { };
    overlays = [ ];
  };
in rec {
  hello = pkgs.callPackage ./01-hello.nix { };
  hello-nix = pkgs.callPackage ./01-hello.nix { audience = "Nix"; };
  hello-folks = hello-nix.override { audience = "folks"; };
}
