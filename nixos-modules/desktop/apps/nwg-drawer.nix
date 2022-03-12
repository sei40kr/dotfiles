{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.desktop.apps.nwg-drawer;

  suspend = pkgs.makeDesktopItem {
    name = "suspend";
    desktopName = "Suspend";
    icon = "system-suspend";
    exec = "systemctl suspend";
    categories = [ "System" ];
  };
  reboot = pkgs.makeDesktopItem {
    name = "reboot";
    desktopName = "Reboot";
    icon = "system-reboot";
    exec = "systemctl reboot";
    categories = [ "System" ];
  };
  powerOff = pkgs.makeDesktopItem {
    name = "power-off";
    desktopName = "Power Off";
    icon = "system-shutdown";
    exec = "systemctl poweroff";
    categories = [ "System" ];
  };
in {
  options.modules.desktop.apps.nwg-drawer = with types; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      nwg-drawer

      suspend
      reboot
      powerOff
    ];

    home.configFile."nwg-drawer/drawer.css".source =
      "${configDir}/nwg-drawer/drawer.css";

    systemd.user.services.nwg-drawer = {
      enable = true;
      description = "Application drawer for sway Wayland compositor";
      after = [ "graphical-session.target" ];
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.nwg-drawer}/bin/nwg-drawer -nofs -ovl -r";
        Restart = "on-failure";
      };
      wantedBy = [ "graphical-session.target" ];
    };
  };
}
