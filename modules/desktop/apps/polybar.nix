{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) makeBinPath mkEnableOption mkIf;
  cfg = config.modules.desktop.apps.polybar;

  polybar-gnome-pomodoro =
    pkgs.runCommand "polybar-gnome-pomodoro"
      {
        src = ../../../bin/polybar-gnome-pomodoro;
        nativeBuildInputs = [
          pkgs.gnused
          pkgs.makeWrapper
        ];
        buildInputs = [ pkgs.jq ];
      }
      ''
        mkdir -p $out/bin
        cp $src $out/bin/polybar-gnome-pomodoro
        sed -i '1,2c#!/usr/bin/env bash' $out/bin/polybar-gnome-pomodoro
        patchShebangs $out
        wrapProgram $out/bin/polybar-gnome-pomodoro \
          --prefix PATH : ${
            makeBinPath [
              pkgs.my.gnome-pomodoro-watcher
              pkgs.jq
            ]
          }
      '';

  configIni = pkgs.substituteAll {
    src = ../../../config/polybar/config.ini;
    polybar_gnome_pomodoro = "${polybar-gnome-pomodoro}/bin/polybar-gnome-pomodoro";
  };
in
{
  options.modules.desktop.apps.polybar = {
    enable = mkEnableOption "Polybar";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ polybarFull ];

    fonts.packages = with pkgs; [ (nerdfonts.override { fonts = [ "NerdFontsSymbolsOnly" ]; }) ];

    systemd.user.services.polybar-top = {
      description = "Polybar top bar as systemd service";
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      restartTriggers = [ configIni ];
      script = "${pkgs.polybarFull}/bin/polybar top";
    };
    systemd.user.services.polybar-bottom = {
      description = "Polybar bottom bar as systemd service";
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      restartTriggers = [ configIni ];
      script = "${pkgs.polybarFull}/bin/polybar bottom";
    };

    environment.etc."xdg/polybar/config.ini".source = configIni;
  };
}
