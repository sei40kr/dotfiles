{
  config,
  lib,
  osConfig,
  pkgs,
  ...
}:

let
  inherit (lib) attrByPath mkIf;

  themeCfg = attrByPath [ "modules" "desktop" "theme" ] { active = null; } osConfig;
  cfg = themeCfg.whitesur or { };
in
{
  config = mkIf (themeCfg.active == "whitesur") {
    gtk = {
      enable = true;
      theme = {
        package = pkgs.whitesur-gtk-theme;
        name = "WhiteSur-${cfg.variant}-solid";
      };
      gtk4.theme = config.gtk.theme;
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
