{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.system.fstrim.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config =
    mkIf config.modules.system.fstrim.enable { services.fstrim.enable = true; };
}
