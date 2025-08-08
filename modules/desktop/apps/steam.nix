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
    ${pkgs.xorg.xset}/bin/xset -dpms
    ${pkgs.xorg.xset}/bin/xset s off
  '';
  end = pkgs.writeShellScriptBin "end" ''
    ${pkgs.xorg.xset}/bin/xset +dpms
    ${pkgs.xorg.xset}/bin/xset s on
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
        # NOTE: Normally this should be set according to the monitor DPI,
        #  but the default Steam UI font size is too small for me, so I always
        #  set it to 125%.
        extraArgs = "-forcedesktopscaling 1.25";
      };
      extraCompatPackages = with pkgs; [ proton-ge-bin ];
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
