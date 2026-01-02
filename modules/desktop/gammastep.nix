{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkIf
    mkEnableOption
    mkPackageOption
    mkOption
    types
    optionalString
    ;
  inherit (types)
    int
    float
    nullOr
    submodule
    ;
  cfg = config.modules.desktop.gammastep;

  gammastepArgs =
    "-t ${toString cfg.temperature.day}:${toString cfg.temperature.night}"
    + optionalString (
      cfg.location != null
    ) " -l ${toString cfg.location.latitude}:${toString cfg.location.longitude}";
in
{
  options.modules.desktop.gammastep = {
    enable = mkEnableOption "Gammastep";

    package = mkPackageOption pkgs "gammastep" { };

    temperature = {
      day = mkOption {
        type = int;
        default = 5700;
        description = "Daytime color temperature in Kelvin.";
      };

      night = mkOption {
        type = int;
        default = 3500;
        description = "Nighttime color temperature in Kelvin.";
      };
    };

    location = mkOption {
      type = nullOr (submodule {
        options = {
          latitude = mkOption {
            type = float;
            description = "Latitude for manual location.";
          };

          longitude = mkOption {
            type = float;
            description = "Longitude for manual location.";
          };
        };
      });
      default = null;
      description = "Manual location coordinates.";
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    systemd.user.services.gammastep = {
      description = "Set color temperature of display according to time of day.";
      documentation = [ "man:gammastep(1)" ];
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = "${cfg.package}/bin/gammastep ${gammastepArgs}";
        RestartSec = 3;
        Restart = "always";
      };
    };
  };
}
