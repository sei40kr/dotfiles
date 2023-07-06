{ config, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.docker;
in
{
  config = mkIf cfg.enable {
    homebrew = {
      enable = true;
      casks = [ "docker" ];
    };
  };
}
