{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.colorschemes;
in {
  config = mkIf (cfg.active == "one-dark") {
    modules.colorschemes.colors = {
      background = "#282c34";
      cursor = "#abb2bf";
      text = "#abb2bf";

      color0 = "#282c34";
      color1 = "#e06c75";
      color2 = "#98c379";
      color3 = "#e5c07b";
      color4 = "#61afef";
      color5 = "#c678dd";
      color6 = "#56b6c2";
      color7 = "#abb2bf";
      color8 = "#3e4452";
      color9 = "#be5046";
      color10 = "#98c379";
      color11 = "#d19a66";
      color12 = "#61afef";
      color13 = "#c678dd";
      color14 = "#56b6c2";
      color15 = "#5c6370";
    };
  };
}
