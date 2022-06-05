{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.ulauncher;
in
{
  options.modules.desktop.apps.ulauncher = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ ulauncher ];

    systemd.user.services.ulauncher = {
      description = "Linux Application Launcher";
      documentation = [ "https://ulauncher.io/" ];
      wantedBy = [ "graphical-session.target" ];
      serviceConfig = {
        Type = "simple";
        Restart = "always";
        RestartSec = 1;
        ExecStart = "${pkgs.ulauncher}/bin/ulauncher --hide-window";
      };
    };
  };
}
