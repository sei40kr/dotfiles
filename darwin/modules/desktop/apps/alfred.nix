{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.alfred;
in
{
  options.modules.desktop.apps.alfred = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    homebrew = {
      enable = true;
      casks = [ "alfred" ];
    };
  };
}
