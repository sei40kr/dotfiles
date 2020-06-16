{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.services.redshift.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.services.redshift.enable {
    services.redshift = { enable = true; };
  };
}
