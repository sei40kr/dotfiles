{ config, lib, pkgs, ... }:

let
  inherit (lib) escapeShellArg mkEnableOption mkIf;
  desktopCfg = config.modules.desktop;
  deCfg = desktopCfg.de;
  cfg = desktopCfg.apps.feh;

  bgMode =
    if deCfg.background.image == null then null
    else if deCfg.background.image.mode == "stretch" then "scale"
    else if deCfg.background.image.mode == "fill" then "fill"
    else if deCfg.background.image.mode == "fit" then "max"
    else if deCfg.background.image.mode == "center" then "center"
    else if deCfg.background.image.mode == "tile" then "tile"
    else throw "Invalid background mode: ${deCfg.background.image.mode}";
in
{
  options.modules.desktop.apps.feh = {
    enable = mkEnableOption "Feh";
  };

  config = mkIf (cfg.enable && deCfg.background.image != null) {
    environment.systemPackages = with pkgs; [ feh ];

    systemd.user.services.feh = {
      description = "Feh";
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      script = "${pkgs.feh}/bin/feh --image-bg ${escapeShellArg deCfg.background.color} --bg-${bgMode} --no-fehbg ${deCfg.background.image.path}";
      serviceConfig = {
        type = "oneshot";
        restart = "on-failure";
      };
    };
  };
}
