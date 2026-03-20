{
  self',
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.services.helloNixosTests;
in {
  options = {
    services.helloNixosTests = {
      enable = lib.mkEnableOption "helloNixosTests";
    };
  };

  #### Implementation

  config = lib.mkIf cfg.enable {
    users.users.hello = {
      createHome = true;
      description = "helloNixosTests user";
      isSystemUser = true;
      group = "hello";
      home = "/srv/helloNixosTests";
    };
    users.groups.hello = {};

    systemd.services.helloNixosTests = {
      description = "helloNixosTests server";
      after = ["network.target"];
      wantedBy = ["multi-user.target"];
      unitConfig = {
        StartLimitIntervalSec = "10sec";
        StartLimitBurst = 1;
      };
      serviceConfig = {
        Type = "exec";
        ExecStart = "${self'.packages.helloNixosTests}/bin/hello-nixos-tests";
        User = "hello";
        Group = "hello";
        Restart = "on-failure";
        RestartSec = "30s";
      };
    };
  };
}
