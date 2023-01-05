{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.alfred;
in
{
  options.modules.desktop.apps.alfred = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ my.alfred ];

    launchd.user.agents.alfred = {
      serviceConfig = {
        Label = "com.runningwithcrayons.Alfred";
        Program = "${pkgs.my.alfred}/Applications/Alfred.app/Contents/MacOS/Alfred";
        KeepAlive = { SuccessfulExit = false; };
        RunAtLoad = true;
      };
    };

    system.defaults.CustomUserPreferences."com.runningwithcrayons.Alfred" = {
      suppressMoveToApplications = true;
    };
  };
}
