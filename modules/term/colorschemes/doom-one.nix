{ config, lib, ... }:

with lib;
with lib.my; {
  config = mkIf (config.modules.term.colorschemes.active == "doom-one") {
    modules.term.colorschemes.colors = {
      fg = "dfdfdf";
      bg = "21242b";

      ansi = {
        black = "1b2229";
        red = "ff6c6b";
        green = "98be65";
        yellow = "ecbe7b";
        blue = "51afef";
        magenta = "c678dd";
        cyan = "46d9ff";
        white = "dfdfdf";
        brightBlack = "1b2229";
        brightRed = "ff6c6b";
        brightGreen = "98be65";
        brightYellow = "ecbe7b";
        brightBlue = "51afef";
        brightMagenta = "c678dd";
        brightCyan = "46d9ff";
        brightWhite = "dfdfdf";
      };

      cursor = {
        fg = "dfdfdf";
        bg = "51afef";
      };

      link = "51afef";

      selection = {
        fg = "dfdfdf";
        bg = "2257a0";
      };

      paneBorder = {
        focused = "191b20";
        default = "191b20";
        urgent = "191b20";
      };

      tabBar = {
        bg = "1d2026";
        activeTab = {
          bg = "51afef";
          fg = "282c34";
        };
        inactiveTab = {
          bg = "21242b";
          fg = "bfbfbf";
        };
      };
    };

    modules.editors.emacs.doom.theme = "doom-one";

    modules.shell.bat.theme = "TwoDark";
  };
}
