{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.services.fstrim.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.services.fstrim.enable {
    services.fstrim.enable = true;
  };
}
