{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.theme;
in
{
  config = mkIf (cfg.active == "whitesur") {
    modules.desktop.gnome = {
      cursor.theme = {
        package = pkgs.my.whitesur-cursors;
        name = "WhiteSur-cursors";
      };
      shell.theme = {
        package = pkgs.whitesur-gtk-theme;
        name = "WhiteSur-light";
      };

      background = {
        image = "${pkgs.my.whitesur-wallpapers}/share/backgrounds/WhiteSur-light.png";
        mode = "zoom";
      };
    };

    modules.desktop.gtk = {
      theme = {
        package = pkgs.whitesur-gtk-theme;
        name = "WhiteSur-light";
      };
      iconTheme = {
        package = pkgs.whitesur-icon-theme;
        name = "WhiteSur";
      };
    };
  };
}
