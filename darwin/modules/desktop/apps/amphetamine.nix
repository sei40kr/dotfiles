{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.apps.amphetamine;
in
{
  options.modules.desktop.apps.amphetamine = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    homebrew = {
      enable = true;
      masApps.Amphetamine = 937984704;
    };
  };
}
