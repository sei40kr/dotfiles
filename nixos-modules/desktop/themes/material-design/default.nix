{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let themeCfg = config.modules.desktop.theme;
in {
  config = mkIf (themeCfg.active == "material-design") {
    modules.desktop = {
      fontconfig.fonts.sansSerif = {
        packages = with pkgs; [ roboto noto-fonts noto-fonts-cjk ];
        names = [ "Roboto" "Noto Sans Mono" "Noto Sans Mono CJK JP" ];
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
