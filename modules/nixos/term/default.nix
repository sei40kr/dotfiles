{ lib, ... }:

with lib;
with lib.my; {
  options.modules.desktop.term = with types; {
    font = {
      family = mkOpt str "monospace";
      size = mkOpt int 12;
    };

    terminal = mkOpt (either path str) null;
  };
}
