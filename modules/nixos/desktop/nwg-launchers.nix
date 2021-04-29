{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.nwg-launchers;
  barTemplate = (optionals config.modules.desktop.sway.enable [{
    name = "Logout";
    exec = "swaymsg exit";
    icon = cfg.theme.nwgbar.logoutIcon;
  }]) ++ [
    {
      name = "Restart";
      exec = "systemctl reboot";
      icon = cfg.theme.nwgbar.restartIcon;
    }
    {
      name = "Shutdown";
      exec = "systemctl -i poweroff";
      icon = cfg.theme.nwgbar.shutdownIcon;
    }
  ];
in {
  options.modules.desktop.nwg-launchers = with types; {
    enable = mkBoolOpt false;
    package = mkOption {
      type = package;
      default = pkgs.nwg-launchers;
      visible = false;
    };
    theme.nwgbar = {
      logoutIcon = mkOpt str "system-log-out-symbolic";
      restartIcon = mkOpt str "system-restart-symbolic";
      shutdownIcon = mkOpt str "system-shutdown-symbolic";
    };
  };

  config = mkIf cfg.enable {
    user.packages = [ cfg.package ];

    # nwgbar
    home.configFile."nwg-launchers/nwgbar/bar.json".text =
      builtins.toJSON barTemplate;
  };
}
