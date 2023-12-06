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
    modules.desktop.background.image = {
      path = "${pkgs.my.whitesur-wallpapers}/share/backgrounds/WhiteSur-${cfg.variant}.png";
      mode = "fit";
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
