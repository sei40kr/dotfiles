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

  orchis-theme = pkgs.orchis-theme.override { withWallpapers = true; };
in
{
  config = mkIf (themeCfg.active == "orchis") {
    gtk = {
      enable = true;
      font = {
        package = pkgs.roboto;
        name = "sans-serif";
        size = 11;
      };
      iconTheme = {
        package = pkgs.tela-icon-theme;
        name = "Tela";
      };
      theme = {
        package = orchis-theme;
        name = "Orchis";
      };
      gtk4.theme = config.gtk.theme;
    };

  };
}
