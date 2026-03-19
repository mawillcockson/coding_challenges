{pkgs}:
(pkgs.lib.evalModules {
  modules = [
    ./01-options.nix
    ./02-config.nix
  ];
}).config
