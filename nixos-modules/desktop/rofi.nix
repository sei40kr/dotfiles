{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.desktop.rofi;
in {
  options.modules.desktop.rofi = with types; { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ rofi ];

    home.configFile = {
      "rofi/config.rasi".source = "${configDir}/rofi/config.rasi";
      "rofi/themes/default.rasi".source =
        "${configDir}/rofi/themes/default.rasi";
      "rofi/powermenu.bash".source = "${configDir}/rofi/powermenu.bash";
    };

    environment.etc."sway/config.d/bindings/rofi.conf".text = ''
      set $menu $DOTFILES_BIN/rofi/appmenu

      # Launch - Application
      bindsym $mod+space exec $menu
    '';
  };
}
