{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) binDir configDir;
  cfg = config.modules.desktop.apps.rofi;
in {
  options.modules.desktop.apps.rofi = with types; { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ rofi dconf jq playerctl xdg-utils ];

    home.configFile = {
      "rofi/config.rasi".text = ''
        configuration {
          modi: "${
            concatStringsSep "," [
              "drun"
              "combi"
              "filebrowser:${binDir}/rofi/file-browser.bash"
              "media:${binDir}/rofi/mpris-menu.bash"
              "pomodoro:${binDir}/rofi/pomodoro-menu.bash"
              "system:${binDir}/rofi/system-menu.bash"
              "vpn:${binDir}/rofi/protonvpn-menu.bash"
              "window:${binDir}/rofi/sway-windows.bash"
            ]
          }";
          drun-display-format: "{name} [<small>({generic})</small>]";
          hover-select: true;
          combi-modi: "drun,media,pomodoro,system,vpn";
          window-format: "{t}";
          theme: "default.rasi";
          combi-hide-mode-prefix: true;
          display-combi: ">";
          me-select-entry: "";
          me-accept-entry: "MousePrimary";
        }
      '';
      "rofi/themes/default.rasi".source =
        "${configDir}/rofi/themes/default.rasi";
    };
  };
}
