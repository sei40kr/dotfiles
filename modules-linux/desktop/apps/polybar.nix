{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.desktop.apps.polybar;
  openweathermapApiKey =
    import <secrets/config/polybar/openweathermap-api-key.nix>;
  openweathermap-pop = pkgs.writeShellScriptBin "openweathermap-pop" ''
    export PATH=${escapeShellArg (makeBinPath (with pkgs; [ curl jq ]))}
    export KEY=${escapeShellArg openweathermapApiKey}

    ${escapeShellArg <config/polybar/scripts/openweathermap-pop>}
  '';
  fcitx-status = pkgs.writeShellScriptBin "fcitx-status" ''
    export PATH="${makeBinPath (with pkgs; [ fcitx ])}"

    ${<config/polybar/scripts/fcitx-status>}
  '';
  protonvpn-status = pkgs.writeShellScriptBin "protonvpn-status" ''
    export PATH="${
      makeBinPath (with pkgs;
        with pkgs.my.python3Packages; [
          coreutils
          gawk
          gnugrep
          procps
          protonvpn-cli
          unixtools.ping
        ])
    }"

    ${<config/polybar/scripts/protonvpn-status>}
  '';
  youtube-music = pkgs.writeShellScriptBin "youtube-music" ''
    export PATH="${makeBinPath (with pkgs; [ xdotool ])}"

    ${<config/polybar/scripts/youtube-music>}
  '';
  polybarConfig = import <config/polybar/config.nix> {
    inherit fcitx-status lib openweathermap-pop protonvpn-status youtube-music;

    gnome-pomodoro = "${pkgs.gnome3.pomodoro}";
    gnome-pomodoro_py = "${<config/polybar/scripts/gnome-pomodoro.py>}";
    fcitx = "${pkgs.fcitx}";
  };
in {
  options.modules.desktop.apps.polybar = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    themeConfig = mkOption { type = with types; either path str; };
  };

  config = mkIf cfg.enable {
    modules.desktop.xmonad.polybarStartCommand =
      "${pkgs.polybar}/bin/polybar top";

    my.packages = with pkgs; [ polybar material-design-icons ];
    my.home.xdg.configFile."polybar/config".text = generators.toINI { }
      (polybarConfig // {
        "section/base".include-file = "${cfg.themeConfig}";
      });
  };
}
