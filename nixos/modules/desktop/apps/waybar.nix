{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.desktop.apps.waybar;
in
{
  options.modules.desktop.apps.waybar = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ waybar ];

    fonts.fonts = with pkgs; [ material-design-icons ];

    environment.etc = {
      "xdg/waybar/config".source = "${configDir}/waybar/config.json";
      "xdg/waybar/style.css".source = "${configDir}/waybar/style.css";
    };

    systemd.user.services.waybar = {
      description = "Highly customizable Wayland bar for Sway and Wlroots based compositors";
      documentation = [ "man:waybar(1)" "https://github.com/Alexays/Waybar/wiki" ];
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.waybar}/bin/waybar";
        Restart = "on-failure";
        RestartSec = 1;
        TimeoutStopSec = 10;
      };
      restartIfChanged = true;
      restartTriggers = [
        (builtins.hashFile "md5" "${configDir}/waybar/config.json")
        (builtins.hashFile "md5" "${configDir}/waybar/style.css")
      ];
    };

    # NOTE: Somehow these services increase the startup time of Waybar.
    #       See Alexays/Waybar#1266
    systemd.user.services.xdg-desktop-portal-gnome.enable = false;
    systemd.user.services.xdg-desktop-portal-gtk.enable = false;
    systemd.user.services.xdg-desktop-portal-rewrite-launchers.enable = false;
  };
}
