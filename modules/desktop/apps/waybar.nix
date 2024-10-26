{
  config,
  lib,
  pkgs,
  ...
}:

with builtins;
with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  desktopCfg = config.modules.desktop;
  wmCfg = desktopCfg.wm;
  cfg = desktopCfg.apps.waybar;

  style_css = pkgs.substituteAll {
    src = ../../../config/waybar/style.css;
    sidePadding = wmCfg.gaps.outer;
  };
in
{
  options.modules.desktop.apps.waybar = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ waybar ];

    fonts.packages = with pkgs; [ material-design-icons ];

    environment.etc = {
      "xdg/waybar/config".source = "${configDir}/waybar/config.json";
      "xdg/waybar/style.css".source = style_css;
    };

    systemd.user.services.waybar = {
      description = "Highly customizable Wayland bar for Sway and Wlroots based compositors";
      documentation = [
        "man:waybar(1)"
        "https://github.com/Alexays/Waybar/wiki"
      ];
      serviceConfig = {
        ExecStart = "${pkgs.waybar}/bin/waybar";
        ExecReload = "${pkgs.coreutils}/bin/kill -SIGUSR2 $MAINPID";
        Restart = "on-failure";
        RestartSec = 1;
        TimeoutStopSec = 10;
      };
      reloadIfChanged = true;
      reloadTriggers = [
        (hashFile "md5" "${configDir}/waybar/config.json")
        style_css
      ];
    };

    # NOTE: Somehow these services increase the startup time of Waybar.
    #       See Alexays/Waybar#1266
    systemd.user.services.xdg-desktop-portal-gtk.enable = false;
    systemd.user.services.xdg-desktop-portal-rewrite-launchers.enable = false;
  };
}
