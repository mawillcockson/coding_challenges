{
  lib ? (import ../../default-nixpkgs.nix).pkgs.lib,
  pkgs ? (import ../../default-nixpkgs.nix).pkgs,
  ...
}: let
  urls = import ./urls.nix;
  dirname-patch = ''
    diff --git a/nix/store/7bn5sj8lvj8fgnzmvsaxmglcvmg97h1y-map.sh/bin/map.sh b/map.sh
    index fa2b57d7d451c413e79d6c42faa441f7cc345aed..e0ebf737e1514db2e86b2349b8f524a9eff13acd
    --- a/nix/store/7bn5sj8lvj8fgnzmvsaxmglcvmg97h1y-map.sh/bin/map.sh
    +++ b/map.sh
    @@ -10,9 +10,9 @@ set -euo pipefail

     keyFile=''${XDG_DATA_HOME:-~/.local/share}/google-api/key

     if [[ ! -f "$keyFile" ]]; then
    -    mkdir -p "$(basename "$keyFile")"
    +    mkdir -p "$(dirname "$keyFile")"
         echo "No Google API key found in $keyFile" >&2
         echo "For getting one, see https://developers.google.com/maps/documentation/maps-static/start#before-you-begin" >&2
         exit 1
     fi
  '';
  map-sh =
    pkgs.lib.overrideDerivation
    (pkgs.writeShellApplication {
      name = "map.sh";
      runtimeInputs = [
        pkgs.coreutils
        pkgs.curl
      ];
      derivationArgs = {
        nativeBuildInputs = [pkgs.breakpointHook];
        prePatch = "breakpointHook";
        patches = [dirname-patch];
        # NOTE::IMPROVEMENT there must be a better way to not run shellcheck
        checkPhase = ''
          runHook preCheck
          ${pkgs.stdenv.shellDryRun} "$target"
          runHook postCheck
        '';
      };
      text = builtins.readFile (
        builtins.fetchurl {
          name = "map.sh";
          url = urls.map;
        }
      );
    })
    (finalAttrs: {
      installPhase = (finalAttrs.installPhase or "") + "\npatchPhase";
    });
  geocode-sh = pkgs.writeShellApplication {
    name = "geocode.sh";
    runtimeInputs = [];
    text = (
      builtins.fetchurl {
        name = "geocode.sh";
        url = urls.geocode;
      }
    );
  };
in {
  options = {
    scripts.output = lib.mkOption {
      type = lib.types.package;
    };
    geocode-sh = lib.mkOption {
      type = lib.types.package;
    };
  };

  config = {
    scripts.output = pkgs.writeShellApplication {
      name = "run-map";
      runtimeInputs = [
        pkgs.curl
        pkgs.feh
        map-sh
      ];
      text = ''
        map.sh size=640x640 scale=2 | feh -
      '';
    };
    inherit geocode-sh;
  };
}
