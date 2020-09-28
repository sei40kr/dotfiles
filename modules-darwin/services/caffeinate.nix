{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.services.caffeinate;
  workHoursInSeconds = (cfg.workHourRange.end - cfg.workHourRange.start) * 3600;
in {
  options.modules.services.caffeinate = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    workdays = mkOption { type = with types; listOf int; };

    workHourRange = mkOption {
      type = with types;
        submodule {
          options = {
            start = mkOption { type = int; };
            end = mkOption { type = int; };
          };
        };
    };
  };

  config = mkIf cfg.enable {
    launchd.user.agents.caffeinate = {
      command = "/usr/bin/caffeinate -dim -t ${toString workHoursInSeconds}";
      serviceConfig.StartCalendarInterval = map (weekday: {
        Weekday = weekday;
        Hour = cfg.workHourRange.start;
      }) cfg.workdays;
    };
  };
}
