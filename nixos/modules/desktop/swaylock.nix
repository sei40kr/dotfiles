{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  desktopCfg = config.modules.desktop;
  cfg = desktopCfg.swaylock;
  fonts = desktopCfg.fonts;

  lockCommand = "${pkgs.swaylock-effects}/bin/swaylock -f";
in
{
  options.modules.desktop.swaylock = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ swaylock-effects ];

    home.configFile."swaylock/config".source = pkgs.substituteAll {
      src = ../../../config/swaylock-effects/config;
      fontName = fonts.ui.name;
    };

    security.pam.services.swaylock = { };

    systemd.user.services.swayidle = {
      description = "Swayidle to trigger idle actions";
      documentation = [ "man:swayidle(1)" ];
      partOf = [ "sway-session.target" ];
      wantedBy = [ "sway-session.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.swayidle}/bin/swayidle -w " +
          "timeout 300 ${escapeShellArg lockCommand} " +
          "timeout 600 'swaymsg \"output * dpms off\"' " +
          "resume 'swaymsg \"output * dpms on\"' " +
          "before-sleep ${escapeShellArg lockCommand}";
      };
    };
  };
}
