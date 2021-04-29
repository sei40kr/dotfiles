{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my; {
  options.modules.theme = with types; {
    active = mkOpt' types.str null "Name of the theme to enable.";
  };
}
