{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.theme.whitesur;
  themeCfg = config.modules.desktop.theme;
in
{
  options.modules.desktop.theme.whitesur = with types; {
    variant = mkOpt (enum [ "light" "dark" ]) "light";
  };

  config = mkIf (themeCfg.active == "whitesur") {
    modules.desktop.background.image = {
      path = "${pkgs.my.whitesur-wallpapers}/share/backgrounds/WhiteSur-${cfg.variant}.png";
      mode = "fit";
    };

    modules.desktop.gnome = {
      cursor.theme = {
        package = pkgs.my.whitesur-cursors;
        name = "WhiteSur-cursors";
      };
      shell.theme = {
        package = pkgs.whitesur-gtk-theme;
        name = "WhiteSur-${cfg.variant}";
      };
    };

    modules.desktop.gtk = {
      theme = {
        package = pkgs.whitesur-gtk-theme;
        name = "WhiteSur-${cfg.variant}-solid";
      };
      iconTheme = {
        package = pkgs.whitesur-icon-theme;
        name = "WhiteSur";
      };
    };

    modules.desktop.apps.ulauncher.theme =
      if cfg.variant == "light" then
        pkgs.my.whitesur-light-ulauncher
      else pkgs.my.whitesur-dark-ulauncher;
  };
}
