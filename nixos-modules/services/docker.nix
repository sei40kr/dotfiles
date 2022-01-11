{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.docker;
in {
  options.modules.services.docker = {
    enable = mkBoolOpt false;
    autoPrune.enable = mkBoolOpt false;
    compose.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ docker-compose ];
    # TODO Use user Docker service
    virtualisation.docker = {
      enable = true;
      autoPrune.enable = cfg.autoPrune.enable;
    };

    modules.shell.aliases = {
      dk = "docker";
    } //
      # FIXME mkIf didn't work for some reason
      (optionalAttrs cfg.compose.enable { dko = "docker-compose"; });
  };
}
