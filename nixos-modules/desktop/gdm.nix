{ config, lib, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.gdm;
in
{
  options.modules.desktop.gdm = with types; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;
      displayManager.gdm.enable = true;
    };
  };
}
