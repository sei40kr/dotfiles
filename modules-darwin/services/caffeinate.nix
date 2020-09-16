{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.services.caffeinate;
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
      command = "/usr/bin/caffeinate -dim -t 3600";
      serviceConfig.StartCalendarInterval = flatten (map (weekday:
        map (hour: {
          Weekday = weekday;
          Hour = hour;
        }) (range cfg.workHourRange.start cfg.workHourRange.end)) cfg.workdays);
    };
  };
}
