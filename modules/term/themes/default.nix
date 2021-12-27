{ config, lib, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.term.theme;
  inherit (cfg) colors;
in {
  options.modules.term.theme = with types; {
    active = mkOpt str null;

    colors = {
      cursor = {
        fg = mkOpt str null;
        bg = mkOpt str null;
      };

      url = mkOpt str null;

      fg = mkOpt str null;
      bg = mkOpt str null;
      selection = {
        fg = mkOpt str null;
        bg = mkOpt str null;
      };

      base0 = mkOpt str null;
      base1 = mkOpt str null;
      base2 = mkOpt str null;
      base3 = mkOpt str null;
      base4 = mkOpt str null;
      base5 = mkOpt str null;
      base6 = mkOpt str null;
      base7 = mkOpt str null;
      base8 = mkOpt str null;
      base9 = mkOpt str null;
      base10 = mkOpt str null;
      base11 = mkOpt str null;
      base12 = mkOpt str null;
      base13 = mkOpt str null;
      base14 = mkOpt str null;
      base15 = mkOpt str null;
    };
  };
}
