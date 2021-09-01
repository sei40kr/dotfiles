{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.desktop.rofi;

  rofi-appmenu = pkgs.writeShellScriptBin "rofi-appmenu" ''
    exec ${pkgs.rofi}/bin/rofi -modi drun \
                               -show drun \
                               -theme ${configDir}/rofi/themes/appmenu.rasi
  '';
  rofi-powermenu_topright =
    pkgs.writeShellScriptBin "rofi-powermenu_topright" ''
      exec ${pkgs.rofi}/bin/rofi -3 \
                                 -modi powermenu:${configDir}/rofi/scripts/powermenu.bash \
                                 -show powermenu \
                                 -theme ${configDir}/rofi/themes/powermenu_topright.rasi
    '';
in {
  options.modules.desktop.rofi = with types; { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages =
      [ pkgs.rofi pkgs.material-icons rofi-appmenu rofi-powermenu_topright ];

    environment.etc."sway/config.d/bindings/rofi.conf".text = ''
      set $menu ${rofi-appmenu}/bin/rofi-appmenu

      # Launch - Application
      bindsym $mod+space exec $menu
    '';
  };
}
