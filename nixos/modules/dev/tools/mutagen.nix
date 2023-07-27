{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.dev.tools.mutagen;
  dockerCfg = config.modules.services.docker;
in
{
  config = mkIf cfg.enable {
    assertions = [{
      assertion = !cfg.compose.enable || dockerCfg.compose.enable;
      message = "Docker Compose should be enabled when using Mutagen Compose.";
    }];

    user.packages = with pkgs; [
      mutagen
      (mkIf cfg.compose.enable mutagen-compose)
    ];
  };
}
