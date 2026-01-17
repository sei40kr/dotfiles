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
  };
}
