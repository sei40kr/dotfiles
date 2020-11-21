{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.desktop.apps.polybar;
  owmApiKey = import <secrets/config/polybar/owm-api-key.nix>;

  youtube-music = pkgs.writeShellScriptBin "youtube-music" ''
    export PATH="${makeBinPath (with pkgs; [ xdotool ])}"

    ${<config/polybar/scripts/youtube-music>}
  '';
  owm-pop = pkgs.writeShellScriptBin "owm-pop" ''
    export PATH=${escapeShellArg (makeBinPath (with pkgs; [ curl jq ]))}
    export KEY=${escapeShellArg owmApiKey}

    ${<config/polybar/scripts/owm-pop>}
  '';
  fcitx-status = pkgs.writeShellScriptBin "fcitx-status" ''
    export PATH="${makeBinPath (with pkgs; [ fcitx ])}"

    ${<config/polybar/polybar-scripts/fcitx-status/fcitx-status>}
  '';
  protonvpn = pkgs.writeShellScriptBin "protonvpn" ''
    export PATH="${
      makeBinPath (with pkgs; with pkgs.my; [ protonvpn-cli-wrapper gawk ])
    }"

    ${<config/polybar/polybar-scripts/protonvpn/protonvpn>}
  '';

  polybarConfig = import <config/polybar/config.nix> {
    inherit fcitx-status lib owm-pop protonvpn youtube-music;

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
