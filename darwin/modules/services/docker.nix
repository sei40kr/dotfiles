{ config, lib, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.services.docker;
in
{
  options.modules.services.docker = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    homebrew = {
      enable = true;
      casks = [ "docker" ];
    };
  };
}
