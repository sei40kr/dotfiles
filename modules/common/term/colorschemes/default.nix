{ config, lib, ... }:

with lib;
with lib.my; {
  options.modules.term.colorschemes = with types; {
    active = mkOpt str null;
    colors = {
      background = mkOpt str null;
      foreground = mkOpt str null;
      cursor = mkOpt str null;
      normal = {
        black = mkOpt str null;
        red = mkOpt str null;
        green = mkOpt str null;
        yellow = mkOpt str null;
        blue = mkOpt str null;
        magenta = mkOpt str null;
        cyan = mkOpt str null;
        white = mkOpt str null;
      };
      bright = {
        black = mkOpt str null;
        red = mkOpt str null;
        green = mkOpt str null;
        yellow = mkOpt str null;
        blue = mkOpt str null;
        magenta = mkOpt str null;
        cyan = mkOpt str null;
        white = mkOpt str null;
      };
    };
  };
}
