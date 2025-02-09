{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    makeBinPath
    mkEnableOption
    mkIf
    mkOption
    types
    ;
  cfg = config.modules.desktop.apps.polybar;

  polybar-gnome-pomodoro =
    pkgs.runCommand "polybar-gnome-pomodoro"
      {
        src = ../../../bin/polybar-gnome-pomodoro;
        nativeBuildInputs = [
          pkgs.gnused
          pkgs.makeWrapper
        ];
        buildInputs = [
          pkgs.my.gnome-pomodoro-watcher
          pkgs.jq
        ];
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

  polybar-openweathermap =
    pkgs.runCommand "polybar-openweathermap"
      {
        src = ../../../bin/polybar-openweathermap;
        nativeBuildInputs = [
          pkgs.gnused
          pkgs.makeWrapper
        ];
        buildInputs = [
          pkgs.curl
          pkgs.jq
        ];
      }
      ''
        mkdir -p $out/bin
        cp $src $out/bin/polybar-openweathermap
        sed -i '1,2c#!/usr/bin/env bash' $out/bin/polybar-openweathermap
        patchShebangs $out
        wrapProgram $out/bin/polybar-openweathermap \
          --prefix PATH : ${
            makeBinPath [
              pkgs.curl
              pkgs.jq
            ]
          }
      '';

  configIni = pkgs.substituteAll {
    src = ../../../config/polybar/config.ini;

    xdg_utils = "${pkgs.xdg-utils}";

    polybar_gnome_pomodoro = "${polybar-gnome-pomodoro}/bin/polybar-gnome-pomodoro";
    polybar_openweathermap = "${polybar-openweathermap}/bin/polybar-openweathermap";
    openweathermap_key_file = config.age.secrets.openweathermap-key.path;
    openweathermap_city_id = cfg.openweathermap.cityId;
  };
in
{
  options.modules.desktop.apps.polybar = {
    enable = mkEnableOption "Polybar";

    openweathermap = {
      cityId = mkOption {
        type = types.int;
        example = 1850147;
        description = "City ID for OpenWeatherMap API";
      };
    };
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

    age.secrets.openweathermap-key = {
      file = ../../../config/polybar/openweathermap.key.age;
      owner = config.user.name;
    };

    environment.etc."xdg/polybar/config.ini".source = configIni;
  };
}
