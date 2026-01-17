{
  lib,
  osConfig,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf;

  themeCfg = osConfig.modules.desktop.theme;

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
  };
}
