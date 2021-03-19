{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.colorschemes;
in {
  config = mkIf (cfg.active == "doom-one") {
    modules = {
      colorschemes.colors = {
        background = "#21242b";
        cursor = "#dfdfdf";
        text = "#dfdfdf";

        color0 = "#1b2229";
        color1 = "#ff6c6b";
        color2 = "#98be65";
        color3 = "#ecbe7b";
        color4 = "#51afef";
        color5 = "#c678dd";
        color6 = "#46d9ff";
        color7 = "#dfdfdf";
        color8 = "#1b2229";
        color9 = "#ff6c6b";
        color10 = "#98be65";
        color11 = "#ecbe7b";
        color12 = "#51afef";
        color13 = "#c678dd";
        color14 = "#46d9ff";
        color15 = "#dfdfdf";
      };
      shell.bat.theme = "TwoDark";
    };
  };
}
