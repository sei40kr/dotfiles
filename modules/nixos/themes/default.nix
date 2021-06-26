{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my; {
  options.modules.theme = with types; {
    active = mkOpt' types.str null "Name of the theme to enable.";

    variant = mkOpt (enum [ "light" "dark" ]) null;

    colors = {
      success = mkOpt str null;
      warning = mkOpt str null;
      danger = mkOpt str null;

      text = mkOpt str null;
      bg = mkOpt str null;
      border = mkOpt str null;

      base = mkOpt str null;

      selection = {
        text = mkOpt str null;
        bg = mkOpt str null;
        border = mkOpt str null;
      };
    };
  };
}
