{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my; {
  options.modules.desktop.term = with types; {
    terminal = mkOpt (either path str) null;
  };
}
