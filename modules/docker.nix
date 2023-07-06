{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.docker;
in
{
  options.modules.services.docker = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    modules.shell.aliases = {
      dk = "docker";
    };
  };
}
