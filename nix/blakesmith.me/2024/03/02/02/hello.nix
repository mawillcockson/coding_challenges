{
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
        StartLimitIntervalSec = "3sec";
        StartLimitBurst = 3;
      };
      serviceConfig = {
        Type = "exec";
        #ExecStart = "${builtins.trace (builtins.attrNames config) self'.packages.helloNixosTests}/bin/hello-nixos-tests";
        ExecStart = "/bin/false";
        User = "hello";
        Group = "hello";
        Restart = "on-failure";
        RestartSec = "30s";
      };
    };
  };
}
