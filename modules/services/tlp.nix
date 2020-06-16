{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.services.tlp.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config =
    mkIf config.modules.services.tlp.enable { services.tlp.enable = true; };
}
