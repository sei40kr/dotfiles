{ config, lib, ... }:

with lib;
with lib.my; {
  config = mkIf (config.modules.term.colorschemes.active == "tokyo-night") {
    modules.term.colorschemes.colors = {
      fg = "c0caf5";
      bg = "1a1b26";

      ansi = {
        black = "1a1b26";
        red = "f7768e";
        green = "9ece6a";
        yellow = "e0af68";
        blue = "7aa2f7";
        magenta = "bb9af7";
        cyan = "7dcfff";
        white = "a9b1d6";
        brightBlack = "414868";
        brightRed = "f7768e";
        brightGreen = "9ece6a";
        brightYellow = "e0af68";
        brightBlue = "7aa2f7";
        brightMagenta = "bb9af7";
        brightCyan = "7dcfff";
        brightWhite = "c0caf5";
      };

      cursor = {
        fg = "1a1b26";
        bg = "c0caf5";
      };

      link = "73daca";

      selection = {
        fg = "c0caf5";
        bg = "33467c";
      };

      paneBorder = {
        focused = "7aa2f7";
        default = "3b4261";
        urgent = "3b4261";
      };

      tabBar = {
        bg = "15161e";

        activeTab = {
          bg = "7aa2f7";
          fg = "1f2335";
        };

        inactiveTab = {
          bg = "292e42";
          fg = "545c7e";
        };
      };
    };

    modules.editors.emacs.doom.theme = "doom-tokyo-night";

    modules.shell.bat.theme = "TwoDark";
  };
}
