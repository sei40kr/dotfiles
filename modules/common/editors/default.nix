{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.editors;
in {
  options.modules.editors = with types; {
    font = {
      family = mkOpt str "monospace";
      size = mkOpt int 12;
    };
  };
}
