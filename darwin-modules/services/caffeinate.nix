{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.services.caffeinate;
  workHoursInSeconds = (cfg.workHourRange.end - cfg.workHourRange.start) * 3600;
in {
  options.modules.services.caffeinate = with types; {
    enable = mkBoolOpt false;
    workdays = mkOpt (listOf int) [ ];
    workHourRange = {
      start = mkOpt int null;
      end = mkOpt int null;
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
