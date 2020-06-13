{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.tools.redshift.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.tools.redshift.enable {
    services.redshift = { enable = true; };
  };
}
