{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.apps.polybar;

  youtube-music = pkgs.writeShellScriptBin "youtube-music" ''
    export PATH="${makeBinPath (with pkgs; [ xdotool ])}"

    ${configDir}/polybar/scripts/youtube-music
  '';

  polybarConfig = import "${configDir}/polybar/config.nix" {
    inherit lib youtube-music;

    gnome-pomodoro = "${pkgs.gnome3.pomodoro}";
    gnome-pomodoro_py = "${configDir}/polybar/scripts/gnome-pomodoro.py";
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

    user.packages = with pkgs; [ polybar material-design-icons ];
    home.configFile."polybar/config".text = generators.toINI { } (polybarConfig
      // {
        "section/base".include-file = "${cfg.themeConfig}";
      });
  };
}
