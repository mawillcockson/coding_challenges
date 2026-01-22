let
  nixpkgs = builtins.findFile [{
    prefix = "nixpkgs";
    path = (import ../default-nixpkgs-url.nix);
  }] "nixpkgs";
  pkgs = import nixpkgs {
    config = { };
    overlays = [ ];
  };
  localCallPackage = pkgs.lib.callPackageWith (pkgs // packages);
  packages = rec {
    create = localCallPackage ./02-create.nix { };
    run_with_env = localCallPackage ./02-run_with_env.nix { };
    verify = localCallPackage ./02-verify.nix { };
  };
in packages
