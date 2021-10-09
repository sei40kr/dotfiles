{ config, lib, ... }:

with lib;
with lib.my; {
  config = mkIf (config.modules.term.theme.colorscheme == "doom-one") {
    modules = {
      term.theme.colors = {
        fg = "#dfdfdf";
        bg = "#21242b";
        cursor = "#dfdfdf";

        black = "#1b2229";
        red = "#ff6c6b";
        green = "#98be65";
        yellow = "#ecbe7b";
        blue = "#51afef";
        magenta = "#c678dd";
        cyan = "#46d9ff";
        white = "#dfdfdf";
      };
      shell.bat.theme = "TwoDark";
    };
  };
}
