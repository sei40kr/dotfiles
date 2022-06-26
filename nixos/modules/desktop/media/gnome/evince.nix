{ config, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.media.gnome.evince;
in
{
  options.modules.desktop.media.gnome.evince = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    programs.evince.enable = mkForce true;
  };
}
