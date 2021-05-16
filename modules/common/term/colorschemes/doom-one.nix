{ config, lib, ... }:

with lib;
with lib.my;
let active = config.modules.term.colorschemes.active == "doom-one";
in {
  config = mkIf active {
    modules = {
      term.colorschemes.colors = {
        background = "#21242b";
        foreground = "#dfdfdf";
        cursor = "#dfdfdf";
        normal = {
          black = "#1b2229";
          red = "#ff6c6b";
          green = "#98be65";
          yellow = "#ecbe7b";
          blue = "#51afef";
          magenta = "#c678dd";
          cyan = "#46d9ff";
          white = "#dfdfdf";
        };
        bright = {
          black = "#1b2229";
          red = "#ff6c6b";
          green = "#98be65";
          yellow = "#ecbe7b";
          blue = "#51afef";
          magenta = "#c678dd";
          cyan = "#46d9ff";
          white = "#dfdfdf";
        };
      };
      shell.bat.theme = "TwoDark";
    };
  };
}
