let
  nixpkgs-url = (import ../../default-nixpkgs-url.nix);
  nixpkgs = builtins.findFile [{
    prefix = "nixpkgs";
    path = nixpkgs-url;
  }] "nixpkgs";
  pkgs = import nixpkgs {
    config = { };
    overlays = [ ];
  };

in pkgs.mkShellNoCC {
  packages = [ pkgs.cowsay pkgs.lolcat ];

  GREETING = "Hello, Nix!";
  Greeting = "case insensitivity";

  shellHook = ''
    echo "$GREETING" | cowsay | lolcat
  '';
}
