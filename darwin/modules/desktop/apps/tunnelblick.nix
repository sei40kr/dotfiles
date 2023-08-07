{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.tunnelblick;
in
{
  options.modules.desktop.apps.tunnelblick = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    homebrew = {
      enable = true;
      casks = [ "tunnelblick" ];
    };
  };
}
