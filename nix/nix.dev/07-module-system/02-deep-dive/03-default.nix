{lib ? (import ../../default-nixpkgs.nix).pkgs.lib, ...}: {
  options = {
    scripts.output = lib.mkOption {
      type = lib.types.lines;
    };
  };

  config = {
    scripts.output = ''
      ./map.sh size=640x640 scale=2 | feh -
    '';
  };
}
