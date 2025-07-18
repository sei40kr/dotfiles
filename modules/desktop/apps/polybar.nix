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

  polybar-google-calendar-next-event =
    pkgs.runCommand "polybar-google-calendar-next-event"
      {
        src = ../../../bin/polybar-google-calendar-next-event;
        nativeBuildInputs = [
          pkgs.gnused
          pkgs.makeWrapper
        ];
        buildInputs = [
          pkgs.curl
          pkgs.jq
          pkgs.openssl
        ];
      }
      ''
        mkdir -p $out/bin
        cp $src $out/bin/polybar-google-calendar-next-event
        sed -i '1,2c#!/usr/bin/env bash' $out/bin/polybar-google-calendar-next-event
        patchShebangs $out
        wrapProgram $out/bin/polybar-google-calendar-next-event \
          --set SERVICE_ACCOUNT_FILE ${config.age.secrets.google-calendar-service-account.path} \
          --prefix PATH : ${
            makeBinPath [
              pkgs.curl
              pkgs.jq
              pkgs.openssl
            ]
          }
      '';

  polybar-gnome-pomodoro =
    pkgs.runCommand "polybar-gnome-pomodoro"
      {
        src = ../../../bin/polybar-gnome-pomodoro;
        nativeBuildInputs = [
          pkgs.gnused
          pkgs.makeWrapper
        ];
        buildInputs = [
          pkgs.glib
          pkgs.rofi
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
              pkgs.glib
              pkgs.rofi
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

  polybar-wireguard =
    pkgs.runCommand "polybar-wireguard"
      {
        src = ../../../bin/polybar-wireguard;
        nativeBuildInputs = [
          pkgs.gnused
          pkgs.makeWrapper
        ];
        buildInputs = [
          pkgs.gnugrep
          pkgs.iproute2
          pkgs.wireguard-tools
        ];
      }
      ''
        mkdir -p $out/bin
        cp $src $out/bin/polybar-wireguard
        sed -i '1,2c#!/usr/bin/env bash' $out/bin/polybar-wireguard
        patchShebangs $out
        wrapProgram $out/bin/polybar-wireguard \
          --prefix PATH : ${
            makeBinPath [
              pkgs.gnugrep
              pkgs.iproute2
              pkgs.wireguard-tools
            ]
          }
      '';

  configIni = pkgs.replaceVars ../../../config/polybar/config.ini {
    xdg_utils = "${pkgs.xdg-utils}";

    polybar_gnome_pomodoro = "${polybar-gnome-pomodoro}/bin/polybar-gnome-pomodoro";
    polybar_google_calendar_next_event = "${polybar-google-calendar-next-event}/bin/polybar-google-calendar-next-event";
    polybar_openweathermap = "${polybar-openweathermap}/bin/polybar-openweathermap";
    polybar_wireguard = "${polybar-wireguard}/bin/polybar-wireguard";
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

    fonts.packages = with pkgs; [ nerd-fonts.symbols-only ];

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

    age.secrets.google-calendar-service-account = {
      file = ../../../config/polybar/google_calendar_service_account.json.age;
      owner = config.user.name;
    };
    age.secrets.openweathermap-key = {
      file = ../../../config/polybar/openweathermap.key.age;
      owner = config.user.name;
    };

    environment.etc."xdg/polybar/config.ini".source = configIni;
  };
}
