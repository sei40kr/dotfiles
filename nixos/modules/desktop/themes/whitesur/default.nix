{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.theme.whitesur;
  themeCfg = config.modules.desktop.theme;

  Variant = toUpper (substring 0 1 cfg.variant) + substring 1 (-1) cfg.variant;
in
{
  options.modules.desktop.theme.whitesur = with types; {
    variant = mkOpt (enum [ "light" "dark" ]) "light";
  };

  config = mkIf (themeCfg.active == "whitesur") {
    modules.desktop.de.background.image = {
      path = "${pkgs.my.whitesur-wallpapers}/share/backgrounds/WhiteSur-${cfg.variant}.png";
      mode = "fit";
    };

    modules.desktop.apps.dunst = {
      padding = {
        x = 12;
        y = 8;
        textIcon = 8;
      };

      borderWidth = 0;

      icon.size = {
        min = 48;
        max = 48;
      };

      cornerRadius = 12;

      normal = {
        background = if cfg.variant == "light" then "#f5f5f5" else "#333333";
        foreground = if cfg.variant == "light" then "#363636" else "#dadada";
      };
    };

    modules.desktop.gtk = {
      theme = {
        package = pkgs.whitesur-gtk-theme;
        name = "WhiteSur-${cfg.variant}-solid";
      };
      iconTheme = {
        package = pkgs.whitesur-icon-theme.override {
          alternativeIcons = true;
          boldPanelIcons = true;
        };
        name = "WhiteSur";
      };
    };

    modules.desktop.qt.kvantum.theme = {
      package = pkgs.my.whitesur-kde;
      dir = "WhiteSur-solid";
      name = "WhiteSur-solid";
    };
  };
}
