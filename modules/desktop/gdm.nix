{ config, lib, ... }:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;
  cfg = config.modules.desktop.gdm;
in
{
  options.modules.desktop.gdm = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;
      displayManager.gdm = {
        enable = true;
        settings = {
          "org/gnome/settings-daemon/plugins/power" = {
            sleep-inactive-battery-timeout = 0;
          };
        };
      };
    };
  };
}
