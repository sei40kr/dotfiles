{ lib, ... }:

with lib;
with lib.my; {
  options.modules.editors = with types; {
    fonts = {
      code = {
        family = mkOpt str "monospace";
        size = mkOpt int 12;
      };
      ui = {
        family = mkOpt str "sans-serif";
        size = mkOpt int 11;
      };
    };
  };
}
