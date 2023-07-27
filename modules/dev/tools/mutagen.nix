{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.dev.tools.mutagen;
  dockerCfg = config.modules.services.docker;
in
{
  options.modules.dev.tools.mutagen = {
    enable = mkBoolOpt false;

    compose.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    assertions = [{
      assertion = !cfg.enable || dockerCfg.enable;
      message = "Docker should be enabled when using Mutagen.";
    }];
  };
}
