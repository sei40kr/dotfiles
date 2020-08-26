{ config, lib, pkgs, ... }:

with lib; {
  options.modules.services.cupsd.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.services.cupsd.enable {
    services.printing = {
      enable = true;
      webInterface = true;
    };
  };
}
