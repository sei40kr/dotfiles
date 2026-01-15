{
  lib,
  osConfig,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf;

  themeCfg = osConfig.modules.desktop.theme;
  cfg = themeCfg.whitesur;
in
{
  config = mkIf (themeCfg.active == "whitesur") {
    gtk = {
      enable = true;
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
  };
}
