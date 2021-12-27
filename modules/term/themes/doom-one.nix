{ config, lib, ... }:

with lib;
with lib.my; {
  config = mkIf (config.modules.term.theme.active == "doom-one") {
    modules.term.theme.colors = {
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

      cursor = {
        fg = "dfdfdf";
        bg = "51afef";
      };
      url = "51afef";
    };

    modules.shell.bat.theme = "TwoDark";
  };
}
