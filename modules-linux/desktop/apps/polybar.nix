{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.desktop.apps.polybar;
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
  polybarConfig = import <config/polybar/config.nix> {
    inherit lib protonvpn-status;

    gnome-pomodoro = "${pkgs.gnome3.pomodoro}";
    gnome-pomodoro_py = "${<config/polybar/scripts/gnome-pomodoro.py>}";
    fcitx = "${pkgs.fcitx}";
    fcitx_py = "${<config/polybar/scripts/fcitx.py>}";
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
