{ config, lib, pkgs, ... }:

with builtins;
with lib;
with lib.my;
let
  cfg = config.modules.desktop.apps.ulauncher;

  inherit (cfg) theme;
  themeName =
    if theme != null then (fromJSON (readFile "${theme}/manifest.json")).name
    else null;
in
{
  options.modules.desktop.apps.ulauncher = with types; {
    enable = mkBoolOpt false;

    theme = mkOpt (nullOr package) null;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ ulauncher ];

    home.configFile = mkIf (theme != null) {
      "ulauncher/user-themes/${themeName}" = {
        recursive = true;
        source = theme;
      };
    };

    systemd.user.services.ulauncher = {
      description = "Linux Application Launcher";
      documentation = [ "https://ulauncher.io/" ];
      wantedBy = [ "graphical-session.target" ];
      reloadTriggers = [ cfg.theme ];
      serviceConfig = {
        Type = "simple";
        Restart = "always";
        RestartSec = 1;
        ExecStart = "${pkgs.ulauncher}/bin/ulauncher --hide-window";
        ExecReload = "/bin/kill -HUP $MAINPID";
      };
    };
  };
}
