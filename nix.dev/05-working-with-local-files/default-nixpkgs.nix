{
  system ? builtins.currentSystem,
}:
let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/nixos-25.11.tar.gz?rev=15826d42656cb99d69d429aee8c0ce2a486275d0";
    sha256 = "z5NJPSBwsLf/OfD8WTmh79tlSU8XgIbwmk6qB1/TFzY=";
  };
  pkgs = import nixpkgs {
    config = { };
    overlays = [ ];
    inherit system;
  };
in
{
  inherit nixpkgs;
  inherit pkgs;
}
