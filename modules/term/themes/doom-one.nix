{ config, lib, ... }:

with lib;
with lib.my; {
  config = mkIf (config.modules.term.theme.active == "doom-one") {
    modules.term.theme.colors = {
      cursor = {
        fg = "dfdfdf";
        bg = "51afef";
      };

      url = "51afef";

      border = {
        active = "191b20";
        inactive = "191b20";
        bell = "191b20";
      };

      tab = {
        active = {
          fg = "282c34";
          bg = "51afef";
        };
        inactive = {
          fg = "bfbfbf";
          bg = "21242b";
        };
        bg = "1d2026";
      };

      fg = "dfdfdf";
      bg = "21242b";
      selection = {
        fg = "dfdfdf";
        bg = "2257a0";
      };

      base0 = "1b2229";
      base1 = "ff6c6b";
      base2 = "98be65";
      base3 = "ecbe7b";
      base4 = "51afef";
      base5 = "c678dd";
      base6 = "46d9ff";
      base7 = "dfdfdf";
      base8 = "1b2229";
      base9 = "ff6c6b";
      base10 = "98be65";
      base11 = "ecbe7b";
      base12 = "51afef";
      base13 = "c678dd";
      base14 = "46d9ff";
      base15 = "dfdfdf";
    };

    modules.shell.bat.theme = "TwoDark";
  };
}
