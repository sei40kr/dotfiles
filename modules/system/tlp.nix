{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.system.tlp.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config =
    mkIf config.modules.system.tlp.enable { services.tlp.enable = true; };
}
