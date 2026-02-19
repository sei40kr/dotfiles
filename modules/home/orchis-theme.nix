{
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
    };

    modules.desktop.apps.dunst = {
      padding = {
        x = 18;
        y = 8;
        textIcon = 8;
      };

      borderWidth = 0;

      icon.size = {
        min = 48;
        max = 48;
      };

      cornerRadius = 18;

      normal = {
        background = "#f5f5f5";
        foreground = "#000000de";
      };
    };
  };
}
