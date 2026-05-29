{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.desktop.apps.steam;

  start = pkgs.writeShellScriptBin "start" ''
    ${pkgs.xset}/bin/xset -dpms
    ${pkgs.xset}/bin/xset s off
  '';
  end = pkgs.writeShellScriptBin "end" ''
    ${pkgs.xset}/bin/xset +dpms
    ${pkgs.xset}/bin/xset s on
  '';
in
{
  options.modules.desktop.apps.steam = {
    enable = mkEnableOption "Steam";
  };

  config = mkIf cfg.enable {
    programs.steam = {
      enable = true;
      package = pkgs.steam.override {
        extraEnv = {
          MANGOHUD = true;
        };
        extraArgs = "-forcedesktopscaling 1.25";
      };
      extraCompatPackages = [ pkgs.proton-ge-bin ];
    };

    programs.gamemode = {
      enable = true;
      settings = {
        custom = {
          start = "${start}/bin/start";
          end = "${end}/bin/end";
          timeout = 10;
        };
      };
    };
  };
}
