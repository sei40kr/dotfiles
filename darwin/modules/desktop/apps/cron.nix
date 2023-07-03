{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.cron;
in
{
  options.modules.desktop.apps.cron = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    homebrew = {
      enable = true;
      casks = [ "cron" ];
    };
  };
}
