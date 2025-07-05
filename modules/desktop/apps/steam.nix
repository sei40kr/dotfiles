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
