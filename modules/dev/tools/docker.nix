{ config, lib, options, pkgs, ... }:

with lib;
let cfg = config.modules.dev.tools.docker;
in {
  options.modules.dev.tools.docker.enable = mkOption {
    type = types.bool;
    default = false;
  };

  options.modules.dev.tools.docker.enableAutoPrune = mkOption {
    type = types.bool;
    default = false;
  };

  options.modules.dev.tools.docker.enableCompose = mkOption {
    type = types.bool;
    default = true;
  };

  config = mkIf cfg.enable {
    virtualisation.docker = {
      enable = true;
      autoPrune.enable = cfg.enableAutoPrune;
    };

    my.packages = mkIf cfg.enableCompose (with pkgs; [ docker-compose ]);
  };
}
