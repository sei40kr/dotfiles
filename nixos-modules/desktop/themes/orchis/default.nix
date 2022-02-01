{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  themeCfg = config.modules.desktop.theme;

  orchis-theme = pkgs.orchis-theme.override { withWallpapers = true; };
in {
  config = mkIf (themeCfg.active == "orchis") {
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
        iconTheme = {
          package = pkgs.tela-icon-theme;
          name = "Tela";
        };
        theme = {
          package = orchis-theme;
          name = "Orchis";
        };
      };

      sway.wallpaper = "${orchis-theme}/share/backgrounds/4k.jpg";
    };
  };
}
