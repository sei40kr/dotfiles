{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  themeCfg = config.modules.desktop.theme;
  cfg = themeCfg.orchis;
in {
  options.modules.desktop.theme.orchis = with types; {
    accentColor = mkOpt (enum [
      "default"
      "purple"
      "pink"
      "red"
      "orange"
      "yellow"
      "green"
      "grey"
    ]) "default";
  };

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
        theme = {
          theme = {
            package = pkgs.orchis-theme.override { inherit (cfg) accentColor; };
            name = "Orchis";
          };
          iconTheme = {
            package = pkgs.tela-icon-theme;
            name = "Tela";
          };
        };
      };
      sway.wallpaper =
        "${pkgs.orchis-theme}/share/backgrounds/orchis-wallpaper.jpg";
    };
  };
}
