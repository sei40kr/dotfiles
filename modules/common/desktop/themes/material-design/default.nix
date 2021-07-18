{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let themesCfg = config.modules.desktop.themes;
in {
  config = mkIf (themesCfg.active == "material-design") {
    modules.desktop = {
      gnome.theme = {
        package = pkgs.materia-theme;
        name = "Materia-dark";
      };
      gtk = {
        font = {
          package = pkgs.roboto;
          name = "sans-serif";
          size = 11;
        };
        theme = {
          theme = {
            package = pkgs.materia-theme;
            name = "Materia-dark";
          };
          iconTheme = {
            package = pkgs.papirus-icon-theme;
            name = "Papirus";
          };
        };
      };
    };
  };
}
