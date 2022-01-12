{ config, lib, ... }:

with lib;
with lib.my; {
  config = mkIf (config.modules.term.theme.active == "tokyo-night") {
    modules.term.theme.colors = {
      cursor = {
        fg = "1a1b26";
        bg = "c0caf5";
      };

      url = "73daca";

      border = {
        active = "191b20";
        inactive = "191b20";
        bell = "191b20";
      };

      tab = {
        active = {
          fg = "1f2335";
          bg = "7aa2f7";
        };
        inactive = {
          fg = "545c7e";
          bg = "292e42";
        };
        bg = "15161e";
      };

      fg = "c0caf5";
      bg = "1a1b26";
      selection = {
        fg = "c0caf5";
        bg = "33467c";
      };

      base0 = "15161e";
      base1 = "f7768e";
      base2 = "9ece6a";
      base3 = "e0af68";
      base4 = "7aa2f7";
      base5 = "bb9af7";
      base6 = "7dcfff";
      base7 = "a9b1d6";
      base8 = "414868";
      base9 = "f7768e";
      base10 = "9ece6a";
      base11 = "e0af68";
      base12 = "7aa2f7";
      base13 = "bb9af7";
      base14 = "7dcfff";
      base15 = "c0caf5";
    };

    modules.shell.bat.theme = "TwoDark";
  };
}
