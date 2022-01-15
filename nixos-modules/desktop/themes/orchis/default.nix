{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let themeCfg = config.modules.desktop.theme;
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
          package = pkgs.orchis-theme;
          name = "Orchis";
        };
      };

      sway.wallpaper =
        "${pkgs.orchis-theme}/share/backgrounds/orchis-wallpaper.jpg";
    };
  };
}
